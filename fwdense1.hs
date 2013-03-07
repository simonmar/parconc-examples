{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test ) where

import System.Environment
import Data.Array.Repa
import Data.Functor.Identity

-- <<Graph
type Weight = Int
type Graph r = Array r DIM2 Weight
-- >>

-- -----------------------------------------------------------------------------
-- shortestPaths

-- <<shortestPaths
shortestPaths :: Graph U -> Graph U
shortestPaths g0 = runIdentity $ go g0 0                      -- <1>
  where
    Z :. _ :. n = extent g0

    go !g !k | k == n    = return g                           -- <2>
             | otherwise = do
                 g' <- computeP (fromFunction (Z:.n:.n) sp)   -- <3>
                 go g' (k+1)
     where
        sp (Z:.i:.j) = min (g ! (Z:.i:.j)) (g ! (Z:.i:.k) + g ! (Z:.k:.j))
-- >>

-- -----------------------------------------------------------------------------
-- Testing

input :: [[Int]]
input = [[  0, 999, 999,  13, 999, 999],
         [999,   0, 999, 999,   4,   9],
         [ 11, 999,   0, 999, 999, 999],
         [999,   3, 999,   0, 999,   7],
         [ 15,   5, 999,   1,   0, 999],
         [ 11, 999, 999,  14, 999,   0]]

-- correct result:
result :: [[Int]]
result = [[0,  16, 999, 13, 20, 20],
          [19,  0, 999,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, 999,  0,  7,  7],
          [15,  4, 999,  1,  0,  8],
          [11, 17, 999, 14, 21,  0] ]

test :: Bool
test = fromAdjMatrix (shortestPaths (toAdjMatrix input)) == result

toAdjMatrix :: [[Int]] -> Graph U
toAdjMatrix xs = fromListUnboxed (Z :. k :. k) (concat xs)
  where k = length xs

fromAdjMatrix :: Graph U -> [[Int]]
fromAdjMatrix m = chunk k (toList m)
  where
   (Z :. _ :. k) = extent m

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

main :: IO ()
main = do
   [n] <- fmap (fmap read) getArgs
   let g = fromListUnboxed (Z:.n:.n) [1..n^(2::Int)] :: Graph U
   print (sumAllS (shortestPaths g))

