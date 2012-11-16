module TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue) where

import Control.Concurrent.STM

data TBQueue a
   = TBQueue !(TVar Int)  -- CR: read capacity
             !(TVar [a])  -- R:  elements waiting to be read
             !(TVar Int)  -- CW: write capacity
             !(TVar [a])  -- W:  elements written (head is most recent)

-- Total channel capacity is CR + CW. Reads only need to access CR,
-- writes usually need to access only CW but sometimes need CR.  So in
-- the common case we avoid contention between CR and CW.
--
--   - when removing an element from R:
--     CR := CR + 1
--
--   - when adding an element to W:
--     if CW is non-zero
--         then CW := CW - 1
--         then if CR is non-zero
--                 then CW := CR - 1; CR := 0
--                 else **FULL**

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read  <- newTVar []
  write <- newTVar []
  rsize <- newTVar 0
  wsize <- newTVar size
  return (TBQueue rsize read wsize write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue rsize _read wsize write) a = do
  w <- readTVar wsize
  if (w /= 0)
     then do writeTVar wsize (w - 1)
     else do
          r <- readTVar rsize
          if (r /= 0)
             then do writeTVar rsize 0
                     writeTVar wsize (r - 1)
             else retry
  listend <- readTVar write
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue rsize read _wsize write) = do
  xs <- readTVar read
  r <- readTVar rsize
  writeTVar rsize (r + 1)
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return x
    [] -> do
      ys <- readTVar write
      case ys of
        [] -> retry
        _  -> do
          let (z:zs) = reverse ys
          writeTVar write []
          writeTVar read zs
          return z
