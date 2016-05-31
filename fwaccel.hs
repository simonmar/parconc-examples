{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test {-, maxDistances -} ) where

import Prelude
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import AccelerateCompat as A

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
shortestPathsAcc n g0 = foldl1 (>->) steps g0              -- <3>
 where
  steps :: [ Acc Graph -> Acc Graph ]                      -- <1>
  steps =  [ step (unit (constant k)) | k <- [0 .. n-1] ]  -- <2>
-- >>

-- <<step
step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
step k g = generate (shape g) sp                           -- <1>
 where
   k' = the k                                              -- <2>

   sp :: Exp DIM2 -> Exp Weight
   sp ix = let
             (Z :. i :. j) = unlift ix                     -- <3>
           in
             A.min (g ! (index2 i j))                      -- <4>
                   (g ! (index2 i k') + g ! (index2 k' j))
-- >>

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

toAdjMatrix :: [[Weight]] -> Graph
toAdjMatrix xs = A.fromList (Z :. k :. k) (concat xs)
  where k = Prelude.length xs


main :: IO ()
main = do
   (n:_) <- fmap (fmap read) getArgs
   print (run (let g :: Acc Graph
                   g    = generate (constant (Z:.n:.n) :: Exp DIM2) f

                   f :: Exp DIM2 -> Exp Weight
                   f ix = let i,j :: Exp Int
                              Z:.i:.j = unlift ix
                          in
                          A.fromIntegral j +
                           A.fromIntegral i * constant (Prelude.fromIntegral n)
               in
               A.foldAll (+) (constant 0) (shortestPathsAcc n g)))


