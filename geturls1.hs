-- <<geturls
import Control.Concurrent
import Data.ByteString as B
import GetURL

main = do
  m1 <- newEmptyMVar                                    -- <1>
  m2 <- newEmptyMVar                                    -- <1>

  forkIO $ do                                           -- <2>
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  forkIO $ do                                           -- <3>
    r <- getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1                                     -- <4>
  r2 <- takeMVar m2                                     -- <5>
  print (B.length r1, B.length r2)                      -- <6>
-- >>
