-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads
--
-- Compile with:
--    ghc -threaded --make geturls.hs

import GetURL
import TimeIt

import Control.Concurrent
import Text.Printf
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

-- <<async
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- action; putMVar var r)
  return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var
-- >>

-----------------------------------------------------------------------------

-- <<main
sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url   -- <1>
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
 as <- mapM (async . timeDownload) sites  -- <2>
 mapM_ wait as                            -- <3>
-- >>
