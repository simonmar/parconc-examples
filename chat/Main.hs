{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams

   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import ConcurrentUtils

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

{-
Notes

- protocol:
    Server: "Name?"
    Client: <string>
    -- if <string> is already in use, ask for another name
    -- Commands:
    --   /tell <string> message...  (single-user tell)
    --   /quit                      (exit)
    --   /kick <string>             (kick another user)
    --   message...                 (broadcast to all connected users)

- a client needs to both listen for commands from the socket and
  listen for activity from other clients.  Therefore we're going to
  need at least two threads per client (for listening to multiple
  things).  Easiest is to use STM for in-process communication, and to
  have a receiving thread that listens on the socket and forwards to a
  TChan.

- Handle all errors properly, be async-exception safe

- Consistency:
  - if two clients simultaneously kick a third client, only one will be
    successful

See doc/lab-exercises.tex for some ideas for enhancements that you
could try.

-}

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk server handle)
                  (\_ -> hClose handle)

port :: Int
port = 44444
-- >>


-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }
-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server { clients = c }
-- >>

-- <<Message
data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
-- >>

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                }


-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
    clientmap <- readTVar clients
    mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
-- >>

-- <<sendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
    writeTChan clientSendChan msg
-- >>

tell :: Server -> ClientName -> ClientName -> String -> STM ()
tell Server{..} from who msg = do
    clientmap <- readTVar clients
    case Map.lookup who clientmap of
        Nothing -> return ()
        Just client -> sendMessage client $ Tell from msg

kick :: Server -> Client -> ClientName -> STM (IO ())
kick Server{..} client@Client{clientHandle=handle} who = do
    clientmap <- readTVar clients
    case Map.lookup who clientmap of
        Nothing ->
           return $ hPutStrLn handle (who ++ " is not connected")
        Just victim -> do
           writeTVar (clientKicked victim) $ Just ("by " ++ clientName client)
           return $ hPutStrLn handle ("you kicked " ++ who)

-- -----------------------------------------------------------------------------
-- The main server

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
                ok <- checkAddClient server name handle
                case ok of
                  Nothing -> restore $ do
                     hPrintf handle
                        "The name %s is in use, please choose another\n" name
                     readName
                  Just client ->
                     restore (runClient server client)
                       `finally` removeClient server name
-- >>

-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
    clientmap <- readTVar clients
    if Map.member name clientmap
       then return Nothing
       else do client <- newClient name handle
               writeTVar clients (Map.insert name client clientmap)
               broadcast server $ Notice $ name ++ " has connected"
               return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
    modifyTVar' clients $ Map.delete name
    broadcast server $ Notice $ name ++ " has disconnected"
-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..}
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

    receive = forever $ do
       msg <- hGetLine clientHandle
       atomically $ sendMessage client $ Command msg
-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               join $ atomically $ kick server client who
               return True
           "/tell" : who : what -> do
               atomically $ tell server clientName who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast clientName msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>

-- ----------------------------------------------------------------------------
-- Utils

-- | @concurrently left right@ runs @left@ and @right@ in separate
-- threads.  When either @left@ or @right@ completes, whether
-- successfully or by throwing an exception, the other thread is
-- killed with @killThread@, and the call to @concurrently@ returns.

concurrently :: IO () -> IO () -> IO ()
concurrently left right = do
    done <- newEmptyMVar
    mask $ \restore -> do
        let
            spawn x = forkIO $ restore x `finally` tryPutMVar done ()
            stop threads = mapM_ killThread threads
        --
        tids <- mapM spawn [left,right]
        restore (takeMVar done) `onException` stop tids
        stop tids
