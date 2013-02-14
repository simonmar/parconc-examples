module SparseGraph (
    Vertex, Weight,
    AdjMap,
    lookupW, insertW,
    randomGraph,
    mkAdjMap,
    toAdjMatrix,
    fromAdjMatrix,
    checksum
  ) where

import qualified Data.IntMap as Map
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import System.Random
import Data.Array.Unboxed

-- -----------------------------------------------------------------------------
-- Graph representation

type Vertex = Int
type Weight = Int

type AdjMap = IntMap (IntMap Weight)

lookupW :: Vertex -> Vertex -> AdjMap -> Maybe Weight
lookupW i j m = do
  jmap <- Map.lookup i m
  Map.lookup j jmap

insertW :: Vertex -> Vertex -> Weight -> AdjMap -> AdjMap
insertW i j w m = Map.insertWith Map.union i (Map.singleton j w) m

-- -----------------------------------------------------------------------------
-- Testing

randomGraph :: StdGen -> Int -> Int -> Int -> (AdjMap, [Vertex])
randomGraph g max_vertex max_weight edges = (mat, vs)
  where
      (g1,g2) = split g
      (g3,g4) = split g2
      is = take edges $ randomRs (0,max_vertex) g1
      js = take edges $ randomRs (0,max_vertex) g3
      ws = take edges $ randomRs (1,max_weight) g4

      mat = foldr ins Map.empty (zip3 is js ws)
        where ins (i,j,w) = insertW i j w

      vs = IntSet.elems (IntSet.fromList (is ++ js))

mkAdjMap :: [[Int]] -> AdjMap
mkAdjMap xss = Map.fromList (zipWith row [0..] xss)
  where
   row i xs = (i, Map.fromList [ (j, w) | (j,w) <- zip [0..] xs, w /= 100 ])

type AdjMatrix = UArray (Int,Int) Weight

toAdjMatrix :: Int -> AdjMap -> AdjMatrix
toAdjMatrix k m = accumArray (\_ e -> e) 999 ((0,0),(k,k))
      [ ((i,j),w) | (i,jmap) <- Map.toList m, (j,w) <- Map.toList jmap ]

fromAdjMatrix :: AdjMatrix -> [[Int]]
fromAdjMatrix m = chunk (k+1) (elems m)
  where
   (_, (k,_)) = bounds m

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

checksum :: AdjMap -> Int
checksum m = sum (concat (map Map.elems (Map.elems m)))


