{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Distributed.Process
  hiding (Message, mask, finally, handleMessage, proxy)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node as Node hiding (newLocalNode)
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
import System.Environment

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
  deriving (Typeable, Generic)

instance Binary Message
-- >>


-- <<PMessage
data PMessage
  = MsgServerInfo         Bool ProcessId [ClientName]
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

localClientNames :: Map ClientName Client -> [ClientName]
localClientNames m = [ localName c | ClientLocal c <- Map.elems m ]
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

chatServer :: Int -> Process ()
chatServer port = do
  server <- newServer []
  liftIO $ forkIO (socketListener server port)
  spawnLocal (proxy server)
  forever $
    receiveWait
      [ match $ handleRemoteMessage server
      , match $ handleMonitorNotification server
      , matchIf (\(WhereIsReply l _) -> l == "chatServer") $
                handleWhereIsReply server
      , matchAny $ \_ -> return ()      -- discard unknown messages
      ]

handleWhereIsReply _ (WhereIsReply _ Nothing) = return ()
handleWhereIsReply server@Server{..} (WhereIsReply _ (Just pid)) =
  liftIO $ atomically $ do
    clientmap <- readTVar clients
    -- send our own server info,and request a response:
    sendRemote server pid (MsgServerInfo True spid (localClientNames clientmap))

handleMonitorNotification :: Server -> ProcessMonitorNotification -> Process ()
handleMonitorNotification
       server@Server{..} (ProcessMonitorNotification _ pid _) = do
  say (printf "server on %s has died" (show pid))
  liftIO $ atomically $ do
    old_pids <- readTVar servers
    writeTVar servers (filter (/= pid) old_pids)
    clientmap <- readTVar clients
    let
        now_disconnected (ClientRemote RemoteClient{..}) = clientHome == pid
        now_disconnected _ = False

        disconnected_clients = Map.filter now_disconnected clientmap

    writeTVar clients (Map.filter (not . now_disconnected) clientmap)
    mapM_ (deleteClient server) (Map.keys disconnected_clients)

handleRemoteMessage :: Server -> PMessage -> Process ()
handleRemoteMessage server@Server{..} m =
  case m of
    MsgServerInfo rsvp pid clients -> newServerInfo server rsvp pid clients
    MsgSend name msg -> liftIO $ atomically $ void $ sendToName server name msg
    MsgBroadcast msg -> liftIO $ atomically $ broadcastLocal server msg
    MsgKick who by   -> liftIO $ atomically $ kick server who by

    MsgNewClient name pid -> liftIO $ atomically $ do
        ok <- checkAddClient server (ClientRemote (RemoteClient name pid))
        when (not ok) $
          sendRemote server pid (MsgKick name "SYSTEM")

    MsgClientDisconnected name pid -> liftIO $ atomically $ do
         clientmap <- readTVar clients
         case Map.lookup name clientmap of
            Nothing -> return ()
            Just (ClientRemote (RemoteClient _ pid')) | pid == pid' ->
              deleteClient server name
            Just _ ->
              return ()

newServerInfo :: Server -> Bool -> ProcessId -> [ClientName] -> Process ()
newServerInfo server@Server{..} rsvp pid remote_clients = do
  liftIO $ printf "%s received server info from %s\n" (show spid) (show pid)
  join $ liftIO $ atomically $ do
    old_pids <- readTVar servers
    writeTVar servers (pid : filter (/= pid) old_pids)

    clientmap <- readTVar clients

    let new_clientmap = Map.union clientmap $ Map.fromList
               [ (n, ClientRemote (RemoteClient n pid)) | n <- remote_clients ]
            -- ToDo: should remove other remote clients with this pid
            -- ToDo: also deal with conflicts
    writeTVar clients new_clientmap

    when rsvp $ do
      sendRemote server pid
         (MsgServerInfo False spid (localClientNames new_clientmap))

    -- monitor the new server
    return (when (pid `notElem` old_pids) $ void $ monitor pid)

remotable ['chatServer]

port :: Int
port = 44444

master :: Backend -> String -> Process ()
master backend port = do

  mynode <- getSelfNode

  peers0 <- liftIO $ findPeers backend 1000000
  let peers = filter (/= mynode) peers0

  say ("peers are " ++ show peers)

  mypid <- getSelfPid
  register "chatServer" mypid

  forM_ peers $ \peer -> do
    whereisRemoteAsync peer "chatServer"

  chatServer (read port :: Int)

-- <<main
main = do
 [port, chat_port] <- getArgs
 backend <- initializeBackend "localhost" port
                              (Main.__remoteTable initRemoteTable)
 node <- newLocalNode backend
 Node.runProcess node (master backend chat_port)
-- >>

