{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

module ConcurrentUtils (
    -- * Variants of forkIO
    forkFinally,
    forkRepeat
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)
import Control.Monad
import Control.Applicative

import GHC.Exts
import GHC.IO hiding (finally)
import GHC.Conc

-- -----------------------------------------------------------------------------
-- Fork that executes an action at the end

-- | fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

-- | fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
forkRepeat :: IO a -> IO ThreadId
forkRepeat action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO go


tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

