import Control.Concurrent
import Control.Monad

-- <<main
numThreads = 1000000

main = do
  ms <- replicateM numThreads $ do
          m <- newEmptyMVar
          forkIO (putMVar m ())
          return m
  mapM_ takeMVar ms
-- >>
