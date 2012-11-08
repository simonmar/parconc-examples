-- STM Async API used in \secref{stm-async}

module Main where

import GetURL
import TimeIt

import Data.Either
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Control.Applicative
import Text.Printf
import qualified Data.ByteString as B

-- -----------------------------------------------------------------------------
-- STM Async API

-- <<Async
data Async a = Async ThreadId (TMVar (Either SomeException a))
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

-- <<cancel
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled
-- >>

-- <<waitAny
waitAny :: [Async a] -> IO a
waitAny asyncs =
  atomically $ foldr orElse retry (map waitSTM asyncs)
-- >>

-----------------------------------------------------------------------------

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

-- <<main
main = do
  as <- mapM (async . download) sites
  (url,_) <- waitAny as
  printf "%s was first\n" url
 where
  download url = do
     contents <- getURL url
     return (url, contents)
-- >>
