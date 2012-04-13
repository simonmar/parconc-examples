{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams

   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import qualified Data.Foldable  as F

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

Ideas for exercises:

  - add a "/names" command to list the currently connected users

  - add a "/nick" command to change the current client's name.
    Careful! The name is stored as an immutable value in the Client
    record.  What is the best way to make it modifiable?

  - flood prevention: prevent a client from issuing more than a
    certain number of messages in a given time.
-}

port :: Int
port = fromIntegral 1234

main :: IO ()
main = do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s" host (show port)
    forkIO $ serve server handle `finally` hClose handle


-- ---------------------------------------------------------------------------
-- Data structures and initialisation

type ClientName = String

data Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server { clients = c }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                }

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  , clientKicked   :: TVar (Maybe String)
  }


-- -----------------------------------------------------------------------------
-- Basic operations

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg =
    readTVar clients >>= F.mapM_ (\client -> sendMessage client msg)

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
    writeTChan clientSendChan msg

tell :: Server -> ClientName -> ClientName -> String -> STM ()
tell Server{..} from who msg = do
    clientmap <- readTVar clients
    case Map.lookup who clientmap of
        Nothing -> return ()
        Just client -> sendMessage client $ Tell from msg

kick :: Server -> Client -> ClientName -> STM ()
kick Server{..} client who = do
    clientmap <- readTVar clients
    case Map.lookup who clientmap of
        Nothing ->
           sendMessage client $ Notice (who ++ " is not connected")
        Just victim -> do
           writeTVar (clientKicked victim) $ Just ("by " ++ who)
           sendMessage client $ Notice ("you kicked " ++ who)

-- -----------------------------------------------------------------------------
-- The main server

serve :: Server -> Handle -> IO ()
serve server@Server{..} handle = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle LineBuffering
    readName
 where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
         then readName
         else mask $ \restore -> do
                ok <- atomically $ checkAddClient name
                case ok of
                  Nothing -> restore $ do
                     printf "The name %s is in use, please choose another" name
                     readName
                  Just client ->
                     restore (runClient server client)
                       `finally` deleteClient name

    checkAddClient name = do
        clientmap <- readTVar clients
        if Map.member name clientmap
           then return Nothing
           else do client <- newClient name handle
                   modifyTVar' clients $ Map.insert name client
                   broadcast server $ Notice $ name ++ " has connected"
                   return (Just client)

    deleteClient name = atomically $ do
        modifyTVar' clients $ Map.delete name
        broadcast server $ Notice $ name ++ " has disconnected"


runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..}
 = concurrently send receive
 where
    send = join $ atomically $ do
        k <- readTVar clientKicked
        case k of
            Just reason -> return $
                hPutStrLn clientHandle $ "You have been kicked: " ++ reason
            Nothing -> do
                msg <- readTChan clientSendChan
                return $ do
                    handleMessage client msg
                    send

    receive = do
       msg <- hGetLine clientHandle
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server client who
               receive
           "/tell" : who : what -> do
               atomically $ tell server clientName who (unwords what)
               receive
           ["/quit"] -> return ()
           ('/':_):_ -> do
               atomically $ sendMessage client $
                               Notice ("Unrecognised command: " ++ msg)
               receive
           _ -> do
               atomically $ broadcast server $ Broadcast clientName msg
               receive


handleMessage :: Client -> Message -> IO ()
handleMessage Client{..} message =
    hPutStrLn clientHandle $
        case message of
            Notice msg         -> "*** " ++ msg
            Tell name msg      -> "*" ++ name ++ "*: " ++ msg
            Broadcast name msg -> "<" ++ name ++ ">: " ++ msg

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
