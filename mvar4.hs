import Control.Concurrent
import GHC.Conc

-- <<main
main = do
  t <- myThreadId
  labelThread t "main"
  m <- newEmptyMVar
  t <- forkIO $ putMVar m 'a'
  labelThread t "a"
  t <- forkIO $ putMVar m 'b'
  labelThread t "b"
  takeMVar m
  takeMVar m
-- >>
