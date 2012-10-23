-- <<fork
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering -- <1>
  forkIO $ forever $ putChar 'A'   -- <2>
  forkIO $ forever $ putChar 'B'   -- <2>
  threadDelay (10^6)               -- <3>
-- >>
