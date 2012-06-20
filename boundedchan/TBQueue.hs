module TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue) where

import Control.Concurrent.STM

data TBQueue a = TBQueue !(TVar Int) !(TVar [a])
                         !(TVar Int) !(TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read  <- newTVar []
  write <- newTVar []
  rsize <- newTVar 0
  wsize <- newTVar size
  return (TBQueue rsize read wsize write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue rsize _read wsize write) a = do
  listend <- readTVar write
  w <- readTVar wsize
  if (w /= 0)
     then do writeTVar wsize (w - 1)
     else do
          r <- readTVar rsize
          if (r == 0)
             then retry
             else do writeTVar rsize 0
                     writeTVar wsize (r - 1)
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue rsize read _wsize write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do r <- readTVar rsize
                  writeTVar rsize (r + 1)
                  writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry
               _  -> do let (z:zs) = reverse ys
                        writeTVar write []
                        writeTVar read zs
                        r <- readTVar rsize
                        writeTVar rsize (r + 1)
                        return z
