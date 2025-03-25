import ConcurrentUtils
import NetworkUtils
import Control.Monad
import Control.Concurrent (forkIO)
import System.IO
import Text.Printf
import Control.Exception

-- <<main
main = withSocketsDo $ do
    printf "Listening on port %d\n" port
    listenOn port $ \sock ->                                      -- <1>
      forever $                                                   -- <2>
        accept sock $ \(handle, peer) -> do
          printf "Accepted connection from %s\n" (show peer)
          forkFinally (talk handle) (\_ -> hClose handle)             -- <4>

port :: Int
port = 44444
-- >>

-- <<talk
talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering                                -- <1>
  loop                                                         -- <2>
 where
  loop = do
    line <- hGetLine h                                         -- <3>
    if line == "end"                                           -- <4>
       then hPutStrLn h ("Thank you for using the " ++         -- <5>
                         "Haskell doubling service.")
       else do hPutStrLn h (show (2 * (read line :: Integer))) -- <6>
               loop                                            -- <7>
-- >>
