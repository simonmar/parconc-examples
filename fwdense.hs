{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test, maxDistances ) where

import System.Environment
import Data.Array.Repa

-- <<Graph
type Weight = Int
type Graph r = Array r DIM2 Weight
-- >>

-- -----------------------------------------------------------------------------
-- shortestPaths

-- <<shortestPaths
shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0                                                -- <2>
  where
    Z :. _ :. n = extent g0                                               -- <1>

    go !g !k | k == n    = g                                              -- <3>
             | otherwise =
                 let g' = computeS (fromFunction (Z:.n:.n) sp)            -- <4>
                 in  go g' (k+1)                                          -- <5>
     where
       sp (Z:.i:.j) = min (g ! (Z:.i:.j)) (g ! (Z:.i:.k) + g ! (Z:.k:.j)) -- <6>
-- >>

-- -----------------------------------------------------------------------------

-- <<maxDistance
maxDistance :: Weight -> Weight -> Weight
maxDistance x y
  | x == inf  = y
  | y == inf  = x
  | otherwise = max x y
-- >>

maxDistances :: Graph U -> Array U DIM1 Weight
maxDistances = foldS maxDistance inf

-- -----------------------------------------------------------------------------
-- Testing

-- <<inf
inf :: Weight
inf = 999
-- >>

testGraph :: Graph U
testGraph = toAdjMatrix $
        [[  0, inf, inf,  13, inf, inf],
         [inf,   0, inf, inf,   4,   9],
         [ 11, inf,   0, inf, inf, inf],
         [inf,   3, inf,   0, inf,   7],
         [ 15,   5, inf,   1,   0, inf],
         [ 11, inf, inf,  14, inf,   0]]

-- correct result:
expectedResult :: Graph U
expectedResult = toAdjMatrix $
         [[0,  16, inf, 13, 20, 20],
          [19,  0, inf,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, inf,  0,  7,  7],
          [15,  4, inf,  1,  0,  8],
          [11, 17, inf, 14, 21,  0] ]

test :: Bool
test = shortestPaths testGraph == expectedResult

toAdjMatrix :: [[Weight]] -> Graph U
toAdjMatrix xs = fromListUnboxed (Z :. k :. k) (concat xs)
  where k = length xs

main :: IO ()
main = do
   [n] <- fmap (fmap read) getArgs
   let g = fromListUnboxed (Z:.n:.n) [0..n^(2::Int)-1] :: Graph U
   print (sumAllS (shortestPaths g))

