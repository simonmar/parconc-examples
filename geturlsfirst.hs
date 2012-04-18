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

-- | An asynchronous action spawned by 'async'.  Asynchronous actions
-- are executed in a separate thread, and operations are provided for
-- waiting for asynchronous actions to complete and obtaining their
-- results (see e.g. 'wait').
--
data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkIO (do r <- try action; atomically (putTMVar var r))
  return (Async t var)

wait :: Async a -> IO (Either SomeException a)
wait = atomically . waitSTM

waitSTM :: Async a -> STM (Either SomeException a)
waitSTM (Async _ var) = readTMVar var

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

waitAny :: [Async a] -> IO ()
waitAny asyncs =
  atomically $
    foldr1 orElse $
      map (void . waitSTM) asyncs

-----------------------------------------------------------------------------

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

main = do
  as <- mapM (async.http) sites

  waitAny as
  mapM_ cancel as
  rs <- mapM wait as
  printf "%d/%d finished\n" (length (rights rs)) (length rs)
 where
   http url = do
     (page, time) <- timeit $ getURL url
     printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time
