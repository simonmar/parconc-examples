{-# LANGUAGE CPP #-}
-- STM Async API used in \secref{stm-async}

module Main where

import GetURL

#if __GLASGOW_HASKELL__ < 706
import ConcurrentUtils (forkFinally)
#endif
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Text.Printf
import qualified Data.ByteString as B

-- -----------------------------------------------------------------------------
-- STM Async API

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

-----------------------------------------------------------------------------

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

-- <<main
main :: IO ()
main = do
  let
    download url = do
       r <- getURL url
       return (url, r)

  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as
-- >>
