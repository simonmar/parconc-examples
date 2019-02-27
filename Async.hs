module Async (
    Async,
    async,
    withAsync,
    cancel,
    waitCatch,
    waitCatchSTM,
    waitSTM,
    wait,
    waitEither,
    waitAny,
    waitBoth,
    concurrently
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent (ThreadId, forkIO)

-- <<Async
data Async a = Async ThreadId (STM (Either SomeException a))
-- >>

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

-- <<async
async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t (readTMVar var))
-- >>

-- <<Functor
instance Functor Async where
  fmap f (Async t stm) = Async t stm'
    where stm' = do
            r <- stm
            case r of
              Left e  -> return (Left e)
              Right a -> return (Right (f a))
-- >>

--- <<watchCatch
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM
-- >>

-- <<waitCatchSTM
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ stm) = stm
-- >>

-- <<waitSTM
waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e  -> throwSTM e
    Right a -> return a
-- >>

-- <<wait
wait :: Async a -> IO a
wait = atomically . waitSTM
-- >>

-- <<cancel
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled
-- >>

-- <<waitEither
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
    `orElse`
  fmap Right (waitSTM b)
-- >>

-- <<waitAny
waitAny :: [Async a] -> IO a
waitAny asyncs =
  atomically $ foldr orElse retry $ map waitSTM asyncs
-- >>

-- <<withAsync
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io operation = bracket (async io) cancel operation
-- >>

-- <<waitBoth
waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth a1 a2 =
  atomically $ do
    r1 <- waitSTM a1 `orElse` (do waitSTM a2; retry) -- <1>
    r2 <- waitSTM a2
    return (r1,r2)
-- >>

-- <<concurrently
concurrently :: IO a -> IO b -> IO (a,b)
concurrently ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitBoth a b
-- >>

-- <<race
race :: IO a -> IO b -> IO (Either a b)
race ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitEither a b
-- >>
