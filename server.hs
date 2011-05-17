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
    l <- hGetLine h
    if l == "end"
       then hPutStrLn h "Thank you for using the Haskell doubling service."
       else do hPutStrLn h (show (2 * (read l :: Integer))); loop
