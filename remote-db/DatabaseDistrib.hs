{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
module DatabaseDistrib (
       Database,
       createDB,
       get, set,
       rcdata,
  ) where

import Remote
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import Data.DeriveTH
import qualified Data.Binary
import Data.Typeable
import System.IO.Error
import Data.Char

import qualified Data.Map as Map
import Data.Map (Map)

import Worker

-- <<master
dbProc :: ProcessM ()
dbProc = do
  peers <- getPeers

  let workers = findPeerByRole peers "WORKER"

  ps <- forM workers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid worker__closure

  when (null ps) $ liftIO $ ioError (userError "no workers")

  mypid <- getSelfPid
  sequence_ [ monitorProcess mypid p MaMonitor | p <- ps ]

  let
    n_workers = length workers

    workerForKey :: Key -> ProcessId
    workerForKey k = ps !! (ord (head k) `mod` n_workers)

    handleRequest :: Request -> ProcessM ()
    handleRequest r =
      case r of
        Set k _ -> workerForKey k ! r
        Get k _ -> workerForKey k ! r

  forever $
    receiveWait
        [ match $ \req -> handleRequest req
        , match $ \(ProcessMonitorException pid reason) -> do
            say (printf "process %s died: %s" (show pid) (show reason))
            terminate
        ]

$( remotable ['dbProc] )

type Database = ProcessId

createDB :: ProcessM Database
createDB = do
  here <- getSelfNode
  spawn here dbProc__closure

set :: Database -> Key -> Value -> ProcessM ()
set db k v = db ! Set k v

get :: Database -> Key -> ProcessM (Maybe Value)
get db k = do
  (s,r) <- newChannel
  db ! Get k s
  receiveChannel r

rcdata = [DatabaseDistrib.__remoteCallMetaData, Worker.__remoteCallMetaData]
