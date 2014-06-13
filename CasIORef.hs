{-# LANGUAGE MagicHash, UnboxedTuples #-}
module CasIORef (casIORef) where

import GHC.IORef
import GHC.STRef
import GHC.Exts
import GHC.IO

casIORef :: IORef a -> a -> a -> IO Bool
casIORef (IORef (STRef r#)) old new = IO $ \s ->
   case casMutVar# r# old new s of
     (# s', did, val #) ->
        case did of
             0# -> (# s', False #)
             _  -> (# s', True #)
