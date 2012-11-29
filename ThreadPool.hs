module ThreadPool where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Text.Printf

-- <<ThreadPool
data ThreadPool = ThreadPool [ThreadId] (Chan (IO ()))

newThreadPool :: Int -> IO ThreadPool
newThreadPool n = do
  c <- newChan
  tids <- replicateM n $ forkIO (worker c)
  return (ThreadPool tids c)
 where
  worker c = forever $ join $ readChan c

submit :: ThreadPool -> IO a -> IO (IO a)
submit (ThreadPool _ c) io = do
  m <- newEmptyMVar
  writeChan c (try io >>= putMVar m r)
  return (wait m)
 where
  wait m = do
   r <- takeMVar m
   case r of
     Left e -> throwIO (e :: SomeException)
     Right a -> return a
-- >>

stopGang :: ThreadPool -> IO ()
stopGang (ThreadPool tids _) = mapM_ killThread tids
