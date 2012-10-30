import Control.Concurrent

-- <<main
main = do
  m <- newEmptyMVar
  takeMVar m
-- >>
