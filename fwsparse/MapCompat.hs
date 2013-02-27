{-# LANGUAGE CPP #-}

module MapCompat (module MapCompat, module Map) where

#if !MIN_VERSION_containers(0,5,0)

import Data.IntMap as Map
import Control.Applicative
import Data.Traversable

traverseWithKey :: Applicative f => (Int -> a -> f b) -> IntMap a -> f (IntMap b)
traverseWithKey f m =
  Map.fromList `fmap` traverse (\(k,v) -> (,) <$> pure k <*> f k v) (Map.toList m)

#else

import Data.IntMap.Strict as Map

#endif
