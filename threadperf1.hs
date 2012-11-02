import Control.Concurrent
import Control.Monad

-- <<main
numThreads = 1000000

main = do
  m <- newEmptyMVar
  replicateM_ numThreads $ forkIO (putMVar m ())
  replicateM_ numThreads $ takeMVar m
-- >>
