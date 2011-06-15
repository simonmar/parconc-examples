-- Modified version of kmeans.hs: uses divide-and-conquer instead of flat parList.

import System.IO
import KMeansCommon
import Data.Array
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import Debug.Trace
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment
import Data.Time.Clock
import Control.Exception

main = do
  points <- decodeFile "points.bin"
  clusters <- getClusters "clusters"
  let nclusters = length clusters
  args <- getArgs
  npoints <- evaluate (length points)
  t0 <- getCurrentTime
  final_clusters <- case args of
   ["seq"] -> kmeans_seq nclusters points clusters
   ["par",n] -> kmeans_par (read n) nclusters points clusters npoints
   _other -> error "args"
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

split :: Int -> [a] -> [[a]] 
split numChunks l = splitSize (ceiling $ fromIntegral (length l) / fromIntegral numChunks) l
   where
      splitSize _ [] = []
      splitSize i v = xs : splitSize i ys
        where (xs,ys) = splitAt i v

data Tree a = Leaf a
            | Node (Tree a) (Tree a)


mkPointTree :: Int -> [Vector] -> Int -> Tree [Vector]
mkPointTree depth points npoints
  | depth >= threshold = Leaf points
  | otherwise = Node (mkPointTree (depth+1) xs half)
                     (mkPointTree (depth+1) ys half)
         where
                half = npoints `quot` 2
                (xs,ys) = splitAt half points


threshold = 10 :: Int

kmeans_par :: Int -> Int -> [Vector] -> [Cluster] -> Int -> IO [Cluster]
kmeans_par mappers nclusters points clusters npoints = do
  let
      tree = mkPointTree 0 points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let
             divconq :: Tree [Vector] -> [Cluster]
             divconq (Leaf points) = step nclusters clusters points
             divconq (Node left right) = runEval $ do
                  c1 <- rpar $ divconq left
                  c2 <- rpar $ divconq right
                  rdeepseq c1
                  rdeepseq c2
                  return $! reduce nclusters [c1,c2]

             clusters' = divconq tree

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  --
  final <- loop 0 clusters
  return final



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

tooMany = 50

reduce :: Int -> [[Cluster]] -> [Cluster]
reduce nclusters css =
  concatMap combine (elems (accumArray (flip (:)) [] (0,nclusters) [ (clId c, c) | c <- concat css]))
 where
  combine [] = []
  combine (c:cs) = [foldr combineClusters c cs]


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
