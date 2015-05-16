{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ printf "ping received from %s" (show from)
  mypid <- getSelfPid
  send from (Pong mypid)
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)

-- <<withMonitor
  withMonitor pid $ do
    send pid (Pong mypid)               -- <1>
    receiveWait
      [ match $ \(Pong _) -> do
         say "pong."
         terminate
      , match $ \(ProcessMonitorNotification _ref deadpid reason) -> do
         say (printf "process %s died: %s" (show deadpid) (show reason))
         terminate
      ]
-- >>

-- <<main
main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable
-- >>
