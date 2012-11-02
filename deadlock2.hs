import Control.Concurrent
import Control.Exception as E

-- <<main
main = do
  lock <- newEmptyMVar
  forkIO $ do r <- try (takeMVar lock); print (r :: Either SomeException ())
  threadDelay 1000000
  print (lock == lock)
-- >>
