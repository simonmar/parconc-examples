import GetURL
import TimeIt

import Control.Monad
import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- try action; putMVar var r)  -- <1>
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a) -- <2>
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a -- <3>
wait a = do
  r <- waitCatch a
  case r of
    Left e  -> throwIO e
    Right a -> return a

-- <<waitEither
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
  m <- newEmptyMVar
  forkIO $ do r <- try (fmap Left  (wait a)); putMVar m r
  forkIO $ do r <- try (fmap Right (wait b)); putMVar m r
  wait (Async m)
-- >>

-----------------------------------------------------------------------------

-- <<main
main = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r <- waitEither a1 a2
  print r
-- >>
