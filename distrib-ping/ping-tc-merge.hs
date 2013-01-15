{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import Data.DeriveTH
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
data Message = Ping (SendPort ProcessId)
  deriving Typeable

derive makeBinary ''Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  oneport <- mergePortsBiased ports     -- <1>
  waitForPongs oneport ps               -- <2>

  say "All pongs successfully received"
  terminate

waitForPongs :: ReceivePort ProcessId -> [ProcessId] -> Process ()
waitForPongs _ [] = return ()
waitForPongs port ps = do
  pid <- receiveChan port
  waitForPongs port (filter (/= pid) ps)
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
