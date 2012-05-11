{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
module DatabaseRepl (
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

  -- group the workers:
  let pairs [] = []
      pairs (a:b:xs) = [a,b] : pairs xs
      pairs [x] = []
        -- don't use the last node if we have an odd number

      worker_pairs = pairs ps
      n_slices = length worker_pairs

  loop worker_pairs n_slices


loop :: [[ProcessId]] -> Int -> ProcessM ()
loop worker_pairs n_slices
 = receiveWait
        [ match $ \req -> handleRequest req >> loop worker_pairs n_slices
        , match $ \(ProcessMonitorException pid reason) -> do
            say (printf "process %s died: %s" (show pid) (show reason))
            loop (map (filter (/= pid)) worker_pairs) n_slices
        ]
 where
    workersForKey :: Key -> [ProcessId]
    workersForKey k = worker_pairs !! (ord (head k) `mod` n_slices)

    handleRequest :: Request -> ProcessM ()
    handleRequest r =
      case r of
        Set k _ -> mapM_ (! r) (workersForKey k)
        Get k _ -> mapM_ (! r) (workersForKey k)

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

rcdata = [DatabaseRepl.__remoteCallMetaData, Worker.__remoteCallMetaData]
