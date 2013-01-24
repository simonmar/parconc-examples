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
        if did ==# 0# then (# s', True #)
                      else (# s', False #)

