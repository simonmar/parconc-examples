{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances  #-}
module Worker where

import Remote
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import Data.DeriveTH
import Data.Binary
import Data.Typeable

import qualified Data.Map as Map
import Data.Map (Map)

class Send c a where
   (!) :: Serializable a => c -> a -> ProcessM ()

instance Send ProcessId a where
   (!) = send

instance Send (SendPort a) a where
   (!) = sendChannel

type Key   = String -- should really use ByteString
type Value = String

data Request
  = Set Key Value
  | Get Key (SendPort (Maybe Value))
  deriving Typeable

$( derive makeBinary ''Request )

worker :: ProcessM ()
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

$( remotable ['worker] )
