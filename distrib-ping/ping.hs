{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Static hiding (initRemoteTable)

import DistribUtils

import Text.Printf
import Data.DeriveTH
import Data.Binary
import Data.Typeable

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving Typeable

derive makeBinary ''Message
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

-- <<master
master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node (staticClosure pingServer__static)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong."

  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master (Main.__remoteTable initRemoteTable)
-- >>
