{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test {-, maxDistances -} ) where

import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

-- <<Graph
type Weight = Int
type Graph = Array DIM2 Weight
-- >>

-- -----------------------------------------------------------------------------
-- shortestPaths

-- <<shortestPaths
shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAccel n (use g0))
  where
    Z :. _ :. n = arrayShape g0

shortestPathsAccel :: Int -> Acc Graph -> Acc Graph
shortestPathsAccel n g0 = Prelude.fst r
  where
    r :: (Acc Graph, Acc (Array DIM0 Int))
    r = unlift $ iterate next (lift (g0, unit 0)) !! n

next :: Acc (Graph, Array DIM0 Int) -> Acc (Graph, Array DIM0 Int)
next gk = lift (generate sh sp, unit (k+1))
 where
   !sh = shape g

   !(g,ka) = unlift gk

   !k = the ka

   sp :: Exp DIM2 -> Exp Weight
   sp ix = A.min (g ! (index2 i j)) (g ! (index2 i k) + g ! (index2 k j))
     where (Z :. i :. j) = unlift ix
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

toAdjMatrix :: [[Weight]] -> Graph
toAdjMatrix xs = A.fromList (Z :. k :. k) (concat xs)
  where k = length xs

main :: IO ()
main = do
   [n] <- fmap (fmap read) getArgs
   print (run (let g = generate (lift (Z:.(n::Int):.(n::Int))) f
                   f :: Exp DIM2 -> Exp Int
                   f ix = let i,j :: Exp Int; Z:.i:.j = unlift ix in j
               in
               A.foldAll (+) (constant 0) (shortestPathsAccel n g)))
