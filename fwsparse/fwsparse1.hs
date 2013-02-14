{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, runtest ) where

import Control.Monad.Par
import System.Environment
import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import System.Random
import Data.Traversable hiding (mapM)
import Control.DeepSeq

import SparseGraph

-- -----------------------------------------------------------------------------
-- shortestPaths

shortestPaths :: [Vertex] -> AdjMap -> AdjMap
shortestPaths vs w = go vs w
 where
  go [] w     = w
  go (k:ks) w = go ks $! w'
    where
      w' = runPar $ do
             m <- Map.traverseWithKey (\i jmap -> spawnP (shortmap i jmap)) w
             traverse get m

      shortmap :: Vertex -> IntMap Weight -> IntMap Weight
      shortmap i jmap = foldr shortest Map.empty vs
        where shortest j m =
                case (old,new) of
                   (Nothing, Nothing) -> m
                   (Nothing, Just w ) -> Map.insert j w m
                   (Just w,  Nothing) -> Map.insert j w m
                   (Just w1, Just w2) -> Map.insert j (min w1 w2) m
                where
                  old = Map.lookup j jmap
                  new = do w1 <- lookupW i k w
                           w2 <- lookupW k j w
                           return (w1+w2)

spawnP :: NFData a => a -> Par (IVar a)
spawnP a = do v <- new; fork (put v a); return v

-- -----------------------------------------------------------------------------
-- Testing

test :: [[Int]]
test  = [[  0, 999, 999,  13, 999, 999],
         [999,   0, 999, 999,   4,   9],
         [ 11, 999,   0, 999, 999, 999],
         [999,   3, 999,   0, 999,   7],
         [ 15,   5, 999,   1,   0, 999],
         [ 11, 999, 999,  14, 999,   0]]

-- correct result:
-- [ [0,  16, 999, 13, 20, 20],
--   [19,  0, 999,  5,  4,  9],
--   [11, 27,   0, 24, 31, 31],
--   [18,  3, 999,  0,  7,  7],
--   [15,  4, 999,  1,  0,  8],
--   [11, 17, 999, 14, 21,  0] ]

runtest :: [[Int]]
runtest = fromAdjMatrix (toAdjMatrix 5 (shortestPaths [0..5] (mkAdjMap test)))

main :: IO ()
main = do
  [h,n] <- fmap (fmap read) getArgs
  let g = mkStdGen 9999
  let (mat,vs) = randomGraph g h 100 n
  print (checksum (shortestPaths vs mat))
