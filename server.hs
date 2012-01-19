import Network
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf
import Control.Exception

main =
   withSocketsDo $ do
   mask $ \restore -> do
     s <- listenOn (PortNumber 44444)
     forever $ do
       (h,host,_) <- accept s
       printf "new client: %s\n" host
       forkIO (restore (talk h) `finally` hClose h)

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
 where
  loop = do
    line <- hGetLine h
    if line == "end"
       then hPutStrLn h ("Thank you for using the " ++
                         "Haskell doubling service.")
       else do hPutStrLn h (show (2 * (read line :: Integer))); loop
