{-# LANGUAGE CPP #-}

#if MIN_VERSION_accelerate(0, 14, 0)

module AccelerateCompat (min, max) where

#else

module AccelerateCompat (A.min, A.max) where

import Data.Array.Accelerate as A

#endif
