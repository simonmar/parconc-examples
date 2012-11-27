{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
import Remote
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import Data.DeriveTH
import Data.Binary
import Data.Typeable

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving Typeable

$( derive makeBinary ''Message )
-- >>

-- <<pingServer
pingServer :: ProcessM ()
pingServer = do
  Ping from <- expect
  mypid <- getSelfPid
  send from (Pong mypid)
-- >>

-- <<remotable
$( remotable ['pingServer] )
-- >>

master :: ProcessM ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node pingServer__closure

  mypid <- getSelfPid
  say $ printf "pinging %s" (show pid)

-- <<withMonitor
  withMonitor pid $ do
    send pid (Pong mypid)
    receiveWait
      [ match $ \(Pong _) -> do
         say "pong."
         terminate
      , match $ \(ProcessMonitorException pid reason) -> do
         say (printf "process %s died: %s" (show pid) (show reason))
         terminate
      ]
-- >>

-- <<main
main = remoteInit (Just "config") [Main.__remoteCallMetaData] $
         \_ -> master
-- >>
