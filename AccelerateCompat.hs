{-# LANGUAGE CPP #-}

#if MIN_VERSION_accelerate(1, 0, 0)

module AccelerateCompat (A.min, A.max) where

import Data.Array.Accelerate as A

#elif MIN_VERSION_accelerate(0, 14, 0)

module AccelerateCompat (min, max) where

#else

module AccelerateCompat (A.min, A.max) where

import Data.Array.Accelerate as A

#endif
