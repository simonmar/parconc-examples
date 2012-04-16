module ConcurrentUtils (
  concurrently, concurrentlyEither, concurrentlyBoth) where

import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)
import Control.Monad

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

concurrently :: IO a -> IO b
             -> (MVar (Either SomeException (Either a b)) -> IO r)
             -> IO r
concurrently left right collect = do
    done <- newEmptyMVar
    mask $ \restore -> do
        lid <- forkIO $ restore (left >>= putMVar done . Right . Left)
                             `catchAll` (putMVar done . Left)
        rid <- forkIO $ restore (right >>= putMVar done . Right . Right)
                             `catchAll` (putMVar done . Left)
        let tids = [lid,rid]
        let stop threads = mapM_ killThread threads
        r <- restore (collect done) `onException` stop tids
        stop tids
        return r

-- | @either left right@ runs @left@ and @right@ in separate threads.
-- If @left@ completes first with result @a@, then the result of the
-- call to @either@ will be @Left a@, or if @right@ completes first
-- with result @b@ then the result will be @Right b@.  If either
-- computation throws an exception, then the same exception will be
-- thrown by the call to @either@.  Whichever computation completes or
-- raises an exception first, the other one is killed with
-- @killThread@.

concurrentlyEither :: IO a -> IO b -> IO (Either a b)
concurrentlyEither left right = concurrently left right collect
  where
    collect m = do
        r <- takeMVar m
        case r of
            Left e  -> throwIO e
            Right r -> return r

concurrentlyBoth :: IO a -> IO b -> IO (a,b)
concurrentlyBoth left right = concurrently left right (collect [])
  where
    collect [Left a, Right b] _ = return (a,b)
    collect [Right b, Left a] _ = return (a,b)
    collect xs m = do
        r <- takeMVar m
        case r of
            Left e  -> throwIO e
            Right r -> collect (r:xs) m
