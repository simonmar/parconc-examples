import Control.Concurrent
import GHC.Conc
import Debug.Trace

-- <<main
main = do
  t <- myThreadId
  labelThread t "main"
  m <- newEmptyMVar
  t <- forkIO $ putMVar m 'a'
  labelThread t "a"
  t <- forkIO $ putMVar m 'b'
  labelThread t "b"
  traceEventIO "before takeMVar"
  takeMVar m
  takeMVar m
-- >>
