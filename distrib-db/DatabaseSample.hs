{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings,
    DeriveGeneric #-}
module DatabaseSample (
       Database,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent hiding (newChan)
import GHC.Generics (Generic)
import qualified Data.Binary
import Data.Typeable
import System.IO.Error hiding (catch)
import Data.Char

import qualified Data.Map as Map
import Data.Map (Map)

import WorkerSample

import Prelude hiding (catch)
import Control.Exception hiding (catch)

dbProc :: [NodeId] -> Process ()
dbProc peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'worker)

  when (null ps) $ liftIO $ ioError (userError "no workers")

  mapM_ monitor ps

  -- group the workers:
  let pairs [] = []
      pairs (a:b:xs) = [a,b] : pairs xs
      pairs [x] = []
        -- don't use the last node if we have an odd number

      worker_pairs = pairs ps
      n_slices = length worker_pairs

  loop worker_pairs n_slices


loop :: [[ProcessId]] -> Int -> Process ()
loop worker_pairs n_slices
 = receiveWait
        [ match $ \req -> handleRequest req >> loop worker_pairs n_slices
        , match $ \(ProcessMonitorNotification _ pid reason) -> do
            say (printf "process %s died: %s" (show pid) (show reason))
            loop (map (filter (/= pid)) worker_pairs) n_slices
        ]
 where
    workersForKey :: Key -> [ProcessId]
    workersForKey k = worker_pairs !! (ord (head k) `mod` n_slices)

    handleRequest :: Request -> Process ()
    handleRequest r =
      case r of
        Set k _ -> mapM_ (! r) (workersForKey k)
        Get k _ -> mapM_ (! r) (workersForKey k)

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB peers = spawnLocal (dbProc peers)

set :: Database -> Key -> Value -> Process ()
set db k v = db ! Set k v

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (s,r) <- newChan
  db ! Get k s
  receiveChan r

rcdata = WorkerSample.__remoteTable
