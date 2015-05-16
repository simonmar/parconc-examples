{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Distributed.Process
  hiding (Message, mask, finally, handleMessage, proxy)
import Control.Distributed.Process.Closure
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Network
import System.IO
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable  as F
import Control.Exception

import ConcurrentUtils
import DistribUtils

-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String

data Client
  = ClientLocal   LocalClient
  | ClientRemote  RemoteClient

data RemoteClient = RemoteClient
       { remoteName :: ClientName
       , clientHome :: ProcessId
       }

data LocalClient = LocalClient
       { localName      :: ClientName
       , clientHandle   :: Handle
       , clientKicked   :: TVar (Maybe String)
       , clientSendChan :: TChan Message
       }

clientName :: Client -> ClientName
clientName (ClientLocal  c) = localName c
clientName (ClientRemote c) = remoteName c

newLocalClient :: ClientName -> Handle -> STM LocalClient
newLocalClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return LocalClient { localName      = name
                     , clientHandle   = handle
                     , clientSendChan = c
                     , clientKicked   = k
                     }
-- >>


-- <<Message
data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
  deriving (Typeable, Generic)

instance Binary Message
-- >>


-- <<PMessage
data PMessage
  = MsgServers            [ProcessId]
  | MsgSend               ClientName Message
  | MsgBroadcast          Message
  | MsgKick               ClientName ClientName
  | MsgNewClient          ClientName ProcessId
  | MsgClientDisconnected ClientName ProcessId
  deriving (Typeable, Generic)

instance Binary PMessage
-- >>


-- <<Server
data Server = Server
  { clients   :: TVar (Map ClientName Client)
  , proxychan :: TChan (Process ())
  , servers   :: TVar [ProcessId]
  , spid      :: ProcessId
  }

newServer :: [ProcessId] -> Process Server
newServer pids = do
  pid <- getSelfPid
  liftIO $ do
    s <- newTVarIO pids
    c <- newTVarIO Map.empty
    o <- newTChanIO
    return Server { clients = c, servers = s, proxychan = o, spid = pid }
-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<sendLocal
sendLocal :: LocalClient -> Message -> STM ()
sendLocal LocalClient{..} msg = writeTChan clientSendChan msg
-- >>

-- <<sendRemote
sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote Server{..} pid pmsg = writeTChan proxychan (send pid pmsg)
-- >>

-- <<sendMessage
sendMessage :: Server -> Client -> Message -> STM ()
sendMessage server (ClientLocal client) msg =
    sendLocal client msg
sendMessage server (ClientRemote client) msg =
    sendRemote server (clientHome client) (MsgSend (remoteName client) msg)
-- >>

-- <<sendToName
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
    clientmap <- readTVar clients
    case Map.lookup name clientmap of
        Nothing     -> return False
        Just client -> sendMessage server client msg >> return True
-- >>

-- <<sendRemoteAll
sendRemoteAll :: Server -> PMessage -> STM ()
sendRemoteAll server@Server{..} pmsg = do
    pids <- readTVar servers
    mapM_ (\pid -> sendRemote server pid pmsg) pids
-- >>

-- <<broadcastLocal
broadcastLocal :: Server -> Message -> STM ()
broadcastLocal server@Server{..} msg = do
    clientmap <- readTVar clients
    mapM_ sendIfLocal (Map.elems clientmap)
  where
    sendIfLocal (ClientLocal c)  = sendLocal c msg
    sendIfLocal (ClientRemote _) = return ()
-- >>

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast server@Server{..} msg = do
    sendRemoteAll server (MsgBroadcast msg)
    broadcastLocal server msg
-- >>

-- <<tell
tell :: Server -> LocalClient -> ClientName -> String -> IO ()
tell server@Server{..} LocalClient{..} who msg = do
  ok <- atomically $ sendToName server who (Tell localName msg)
  if ok
     then return ()
     else hPutStrLn clientHandle (who ++ " is not connected.")
-- >>

-- <<kick
kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just (ClientLocal victim) -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)
    Just (ClientRemote victim) -> do
      sendRemote server (clientHome victim) (MsgKick who by)
-- >>

-- -----------------------------------------------------------------------------
-- Handle a local client

talk :: Server -> Handle -> IO ()
talk server@Server{..} handle = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle LineBuffering
    readName
  where
-- <<readName
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
         then readName
         else mask $ \restore -> do
                client <- atomically $ newLocalClient name handle
                ok <- atomically $ checkAddClient server (ClientLocal client)
                if not ok
                  then restore $ do
                     hPrintf handle
                        "The name %s is in use, please choose another\n" name
                     readName
                  else do
                     atomically $ sendRemoteAll server (MsgNewClient name spid)
                     restore (runClient server client)
                       `finally` disconnectLocalClient server name
-- >>

checkAddClient :: Server -> Client -> STM Bool
checkAddClient server@Server{..} client = do
    clientmap <- readTVar clients
    let name = clientName client
    if Map.member name clientmap
       then return False
       else do writeTVar clients (Map.insert name client clientmap)
               broadcastLocal server $ Notice $ name ++ " has connected"
               return True

deleteClient :: Server -> ClientName -> STM ()
deleteClient server@Server{..} name = do
    modifyTVar' clients $ Map.delete name
    broadcastLocal server $ Notice $ name ++ " has disconnected"

disconnectLocalClient :: Server -> ClientName -> IO ()
disconnectLocalClient server@Server{..} name = atomically $ do
     deleteClient server name
     sendRemoteAll server (MsgClientDisconnected name spid)

-- <<runClient
runClient :: Server -> LocalClient -> IO ()
runClient serv@Server{..} client@LocalClient{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendLocal client (Command msg)

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage serv client msg
            when continue $ server
-- >>

-- <<handleMessage
handleMessage :: Server -> LocalClient -> Message -> IO Bool
handleMessage server client@LocalClient{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server who localName
               return True
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast localName msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>

-- -----------------------------------------------------------------------------
-- Main server

-- <<socketListener
socketListener :: Server -> Int -> IO ()
socketListener server port = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk server handle)
                  (\_ -> hClose handle)
-- >>

-- <<proxy
proxy :: Server -> Process ()
proxy Server{..} = forever $ join $ liftIO $ atomically $ readTChan proxychan
-- >>

-- <<chatServer
chatServer :: Int -> Process ()
chatServer port = do
  server <- newServer []
  liftIO $ forkIO (socketListener server port)           -- <1>
  spawnLocal (proxy server)                              -- <2>
  forever $ do m <- expect; handleRemoteMessage server m -- <3>
-- >>

-- <<handleRemoteMessage
handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server@Server{..} m = liftIO $ atomically $
  case m of
    MsgServers pids  -> writeTVar servers (filter (/= spid) pids) -- <1>
    MsgSend name msg -> void $ sendToName server name msg         -- <2>
    MsgBroadcast msg -> broadcastLocal server msg                 -- <2>
    MsgKick who by   -> kick server who by                        -- <2>

    MsgNewClient name pid -> do                                   -- <3>
        ok <- checkAddClient server (ClientRemote (RemoteClient name pid))
        when (not ok) $
          sendRemote server pid (MsgKick name "SYSTEM")

    MsgClientDisconnected name pid -> do                          -- <4>
         clientmap <- readTVar clients
         case Map.lookup name clientmap of
            Nothing -> return ()
            Just (ClientRemote (RemoteClient _ pid')) | pid == pid' ->
              deleteClient server name
            Just _ ->
              return ()
-- >>

remotable ['chatServer]

-- <<main
port :: Int
port = 44444

master :: [NodeId] -> Process ()
master peers = do

  let run nid port = do
         say $ printf "spawning on %s" (show nid)
         spawn nid ($(mkClosure 'chatServer) port)

  pids <- zipWithM run peers [port+1..]
  mypid <- getSelfPid
  let all_pids = mypid : pids
  mapM_ (\pid -> send pid (MsgServers all_pids)) all_pids

  chatServer port

main = distribMain master Main.__remoteTable
-- >>
