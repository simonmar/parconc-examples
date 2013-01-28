{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses,
     FlexibleInstances, DeriveGeneric  #-}
module WorkerSample where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure

import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import qualified Data.Map as Map
import Data.Map (Map)

class Send c a where
   (!) :: Serializable a => c -> a -> Process ()

instance Send ProcessId a where
   (!) = send

instance Send (SendPort a) a where
   (!) = sendChan

type Key   = String -- should really use ByteString
type Value = String

data Request
  = Set Key Value
  | Get Key (SendPort (Maybe Value))
  deriving (Typeable, Generic)

instance Binary Request

worker :: Process ()
worker = go Map.empty
  where
  go store = do
    r <- expect
    case r of
      Set k v ->
        go (Map.insert k v store)
      Get k port -> do
        port ! (Map.lookup k store)
        go store

remotable ['worker]
