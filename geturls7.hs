{-# LANGUAGE CPP #-}

import GetURL

import Control.Concurrent
#if __GLASGOW_HASKELL__ < 706
import ConcurrentUtils (forkFinally)
#endif
import Control.Exception
import qualified Data.ByteString as B
import Control.Concurrent.STM

-----------------------------------------------------------------------------
-- Our Async API:

-- <<Async
data Async a = Async ThreadId (TMVar (Either SomeException a))
-- >>

-- <<async
async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t var)
-- >>

--- <<watchCatch
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM
-- >>

-- <<waitCatchSTM
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var
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

-----------------------------------------------------------------------------

-- <<main
main =
  withAsync (getURL "http://www.wikipedia.org/wiki/Shovel") $ \a1 ->
  withAsync (getURL "http://www.wikipedia.org/wiki/Spade")  $ \a2 -> do
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
-- >>
