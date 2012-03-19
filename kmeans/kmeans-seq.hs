-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
--
-- Usage (sequential):
--   $ ./kmeans-par seq

import System.IO
import KMeansCommon
import Data.Array
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import Debug.Trace
import Control.DeepSeq
import System.Environment
import Data.Time.Clock
import Control.Exception

main = do
  points <- decodeFile "points.bin"
  clusters <- getClusters "clusters"
  let nclusters = length clusters
  args <- getArgs
  evaluate (length points)
  t0 <- getCurrentTime
  final_clusters <- case args of
   ["seq"] ->
   -- ["par",n] -> kmeans_par (read n) nclusters points clusters
   _other -> error "args"
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence

kmeans_seq :: Int -> [Vector] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters = do
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let clusters' = step nclusters clusters points
        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  --
  loop 0 clusters

kmeans_par :: Int -> Int -> [Vector] -> [Cluster] -> IO [Cluster]
kmeans_par n nclusters points clusters
   = error "kmeans_par not defined!"
   -- hint: one approach is to divide the points into n sets, call
   -- step on the sets in parallel and combine the results using
   -- 'reduce', below.

tooMany = 50

-- -----------------------------------------------------------------------------
-- Perform one step of the K-Means algorithm

step :: Int -> [Cluster] -> [Vector] -> [Cluster]
step nclusters clusters points
   = makeNewClusters (assign nclusters clusters points)

-- assign each vector to the nearest cluster centre
assign :: Int -> [Cluster] -> [Vector] -> Array Int [Vector]
assign nclusters clusters points =
    accumArray (flip (:)) [] (0, nclusters-1)
       [ (clId (nearest p), p) | p <- points ]
  where
    nearest p = fst $ minimumBy (compare `on` snd)
                          [ (c, sqDistance (clCent c) p) | c <- clusters ]

makeNewClusters :: Array Int [Vector] -> [Cluster]
makeNewClusters arr =
  filter ((>0) . clCount) $
     [ makeCluster i ps | (i,ps) <- assocs arr ]
                        -- v. important: filter out any clusters that have
                        -- no points.  This can happen when a cluster is not
                        -- close to any points.  If we leave these in, then
                        -- the NaNs mess up all the future calculations.

-- Takes the number of clusters, N, and a list of lists of clusters
-- (each list is length N), and combines the lists to produce a final
-- list of N clusters.
--
-- Useful for parallelising the algorithm!
--
reduce :: Int -> [[Cluster]] -> [Cluster]
reduce nclusters css =
  concatMap combine $ elems $
     accumArray (flip (:)) [] (0,nclusters) [ (clId c, c) | c <- concat css]
 where
  combine [] = []
  combine (c:cs) = [foldr combineClusters c cs]
