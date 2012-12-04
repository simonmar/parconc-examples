{-# LANGUAGE TemplateHaskell #-}
module DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import System.Environment
import Network.Socket hiding (shutdown)

import Language.Haskell.TH

distribMain :: Process () -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable

  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend $ \_ -> master
    [ "master" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend $ \_ -> master
    [ "slave" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend

defaultHost = "localhost"
defaultPort = "44444"
