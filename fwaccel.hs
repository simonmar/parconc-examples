{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test {-, maxDistances -} ) where

import Prelude
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
-- import Data.Array.Accelerate.CUDA as C

-- <<Graph
type Weight = Int32
type Graph = Array DIM2 Weight
-- >>

-- -----------------------------------------------------------------------------
-- shortestPaths

-- <<shortestPaths
shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAcc n (use g0))
  where
    Z :. _ :. n = arrayShape g0
-- >>

-- <<shortestPathsAcc
shortestPathsAcc :: Int -> Acc Graph -> Acc Graph
shortestPathsAcc n g0 =
  foldl1 (>->) (Prelude.map step [0 .. n-1]) g0
-- >>

-- <<step
step :: Int -> Acc Graph -> Acc Graph
step k g = generate (shape g) sp                           -- <1>
 where
   sp :: Exp DIM2 -> Exp Weight
   sp ix = let
             (Z :. i :. j) = unlift ix                     -- <2>
           in
             A.min (g ! (index2 i j))                      -- <3>
                   (g ! (index2 i k') + g ! (index2 k' j))

   k' = the (unit (constant k))                            -- <4>
-- >>

-- -----------------------------------------------------------------------------

{-

-- <<maxDistance
maxDistance :: Exp Weight -> Exp Weight -> Exp Weight
maxDistance x y
  | x == inf  = y
  | y == inf  = x
  | otherwise = max x y
-- >>

maxDistances :: Graph -> Acc (Array DIM1 Weight)
maxDistances = A.fold maxDistance inf

-}

-- -----------------------------------------------------------------------------
-- Testing

-- <<inf
inf :: Weight
inf = 999
-- >>

testGraph :: Graph
testGraph = toAdjMatrix $
        [[  0, inf, inf,  13, inf, inf],
         [inf,   0, inf, inf,   4,   9],
         [ 11, inf,   0, inf, inf, inf],
         [inf,   3, inf,   0, inf,   7],
         [ 15,   5, inf,   1,   0, inf],
         [ 11, inf, inf,  14, inf,   0]]

-- correct result:
expectedResult :: Graph
expectedResult = toAdjMatrix $
         [[0,  16, inf, 13, 20, 20],
          [19,  0, inf,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, inf,  0,  7,  7],
          [15,  4, inf,  1,  0,  8],
          [11, 17, inf, 14, 21,  0] ]

test :: Bool
test = toList (shortestPaths testGraph) == toList expectedResult

test2 = shortestPathsAcc n g0
  where
    n  = 5
    g0 = generate (lift (Z:.n:.n)) (A.fromIntegral . A.snd . A.unindex2)



toAdjMatrix :: [[Weight]] -> Graph
toAdjMatrix xs = A.fromList (Z :. k :. k) (concat xs)
  where k = length xs


main :: IO ()
main = do
   (n:_) <- fmap (fmap read) getArgs
   print (run (let g    = generate (constant (Z:.n:.n) :: Exp DIM2) f
                   f ix = let i,j :: Exp Int; Z:.i:.j = unlift ix in A.fromIntegral j
               in
               A.foldAll (+) (constant 0) (shortestPathsAcc n g)))
