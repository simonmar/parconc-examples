module TChan (TChan, newTChan, readTChan, writeTChan) where

import Control.Concurrent.STM hiding (TChan, newTChan, readTChan, writeTChan)

data TChan a = TChan !(TVar [a])

newTChan :: STM (TChan a)
newTChan = do
  v  <- newTVar []
  return (TChan v)

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan v) a = do
  list <- readTVar v
  writeTVar v (list ++ [a])

readTChan :: TChan a -> STM a
readTChan (TChan v) = do
  xs <- readTVar v
  case xs of
    []      -> retry
    (x:xs') -> do writeTVar v xs'
                  return x
