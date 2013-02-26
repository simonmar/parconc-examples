module TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue) where

import Control.Concurrent.STM
  (STM, TVar, newTVar, readTVar, writeTVar, retry)

-- <<TBQueue
data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a]) -- <1>

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read  <- newTVar []
  write <- newTVar []
  cap   <- newTVar size
  return (TBQueue cap read write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue cap _read write) a = do
  avail <- readTVar cap                         -- <2>
  if avail == 0                                 -- <3>
     then retry                                 -- <4>
     else writeTVar cap (avail - 1)             -- <5>
  listend <- readTVar write
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue cap read write) = do
  avail <- readTVar cap                         -- <6>
  writeTVar cap (avail + 1)
  xs <- readTVar read
  case xs of
    (x:xs') -> do writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry
               _  -> do let (z:zs) = reverse ys
                        writeTVar write []
                        writeTVar read zs
                        return z
-- >>
