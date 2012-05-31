{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
import Remote
import Remote.Process (pfinally)
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import Data.DeriveTH
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
  deriving Typeable

$( derive makeBinary ''Message )
-- >>


-- <<PMessage
data PMessage
  = MsgNewClient ClientName ProcessId
  | MsgClientDisconnected ClientName ProcessId
  | MsgKick ClientName ClientName
  | MsgBroadcast Message
  | MsgSend ClientName Message
  | MsgServers [ProcessId]
  deriving Typeable

$( derive makeBinary ''PMessage )
-- >>


-- <<Server
data Server = Server
  { clients   :: TVar (Map ClientName Client)
  , proxychan :: TChan (ProcessM ())
  , servers   :: TVar [ProcessId]
  , spid      :: ProcessId
  }

newServer :: [ProcessId] -> ProcessM Server
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

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast server@Server{..} msg = do
    sendRemoteAll server (MsgBroadcast msg)
    broadcastLocal server msg

broadcastLocal :: Server -> Message -> STM ()
broadcastLocal server@Server{..} msg = do
    clientmap <- readTVar clients
    mapM_ sendIfLocal (Map.elems clientmap)
  where
    sendIfLocal (ClientLocal c)  = sendLocal c msg
    sendIfLocal (ClientRemote _) = return ()
-- >>

sendLocal :: LocalClient -> Message -> STM ()
sendLocal LocalClient{..} msg = writeTChan clientSendChan msg

sendRemote :: Server -> ProcessId -> PMessage -> STM ()
sendRemote Server{..} pid pmsg = writeTChan proxychan (send pid pmsg)

sendToClient :: Server -> Client -> Message -> STM ()
sendToClient server (ClientLocal client) msg =
    sendLocal client msg
sendToClient server (ClientRemote client) msg =
    sendRemote server (clientHome client) (MsgSend (remoteName client) msg)

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
    clientmap <- readTVar clients
    case Map.lookup name clientmap of
        Nothing     -> return False
        Just client -> sendToClient server client msg >> return True

sendRemoteAll :: Server -> PMessage -> STM ()
sendRemoteAll server@Server{..} pmsg = do
    pids <- readTVar servers
    mapM_ (\pid -> sendRemote server pid pmsg) pids

tell :: Server -> LocalClient -> ClientName -> String -> IO ()
tell server@Server{..} LocalClient{..} who str = do
    ok <- atomically $ sendToName server who (Tell localName str)
    if ok
       then return ()
       else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
    clientmap <- readTVar clients
    case Map.lookup who clientmap of
        Nothing ->
            void $ sendToName server by (Notice $ "you kicked " ++ who)
        Just (ClientLocal victim) -> do
            writeTVar (clientKicked victim) $ Just ("by " ++ by)
            void $ sendToName server by (Notice $ "you kicked " ++ who)
        Just (ClientRemote victim) -> do
            sendRemote server (clientHome victim) (MsgKick who by)

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

disconnectLocalClient :: Server -> ClientName -> IO ()
disconnectLocalClient server@Server{..} name = atomically $ do
     deleteClient server name
     sendRemoteAll server (MsgClientDisconnected name spid)

-- <<runClient
runClient :: Server -> LocalClient -> IO ()
runClient server@Server{..} client@LocalClient{..}
 = race_ send receive
 where
    send = join $ atomically $ do
        k <- readTVar clientKicked
        case k of
            Just reason -> return $
                hPutStrLn clientHandle $ "You have been kicked: " ++ reason
            Nothing -> do
                msg <- readTChan clientSendChan
                return $ do
                    continue <- handleMessage server client msg
                    when continue $ send

    receive = do
       msg <- hGetLine clientHandle
       atomically $ sendLocal client $ Command msg
       receive
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

socketListener :: Server -> Int -> IO ()
socketListener server port = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk server handle)
                  (\_ -> hClose handle)

-- listen for actions from the proxychan and perform them
proxy :: Server -> ProcessM ()
proxy Server{..} = forever $ join $ liftIO $ atomically $ readTChan proxychan

chatServer :: Int -> [ProcessId] -> ProcessM ()
chatServer port pids = do
  server <- newServer pids
  liftIO $ forkIO (socketListener server port)
  spawnLocal (proxy server)
  forever (handleRemoteMessage server)

handleRemoteMessage :: Server -> ProcessM ()
handleRemoteMessage server@Server{..} = do
  m <- expect
  liftIO $ atomically $
    case m of
      MsgServers pids -> writeTVar servers (filter (/= spid) pids)
  
      MsgNewClient name pid -> do
          ok <- checkAddClient server (ClientRemote (RemoteClient name pid))
          when (not ok) $
              sendRemote server pid (MsgKick name "SYSTEM")

      MsgClientDisconnected name pid -> do
           clientmap <- readTVar clients
           case Map.lookup name clientmap of
              Nothing -> return ()
              Just (ClientRemote (RemoteClient _ pid')) | pid == pid' ->
                deleteClient server name
              Just _ ->
                return ()
  
      MsgBroadcast msg -> broadcastLocal server msg
      MsgSend name msg -> void $ sendToName server name msg
      MsgKick who by   -> kick server who by

$( remotable ['chatServer] )

port :: Int
port = 44444

initialProcess "MASTER" = do
  peers <- getPeers
  let workers = findPeerByRole peers "WORKER"

  let run nid port = do
         say $ printf "spawning on %s" (show nid)
         spawn nid (chatServer__closure port [])

  pids <- zipWithM run workers [port+1..]
  mypid <- getSelfPid
  forM_ pids $ \pid -> do
    send pid (MsgServers (mypid:pids))
  
  chatServer port (filter (/= mypid) pids)


initialProcess "WORKER" = receiveWait []


main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess
-- >>
