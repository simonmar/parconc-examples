-- <<fork
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering            -- <1>
  forkIO (replicateM_ 100000 (putChar 'A'))   -- <2>
  replicateM_ 100000 (putChar 'B')            -- <3>
-- >>
