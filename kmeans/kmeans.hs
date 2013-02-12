-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
--
-- With three versions:
--   [ kmeans_seq   ]  a sequential version
--   [ kmeans_strat ]  a parallel version using Control.Parallel.Strategies
--   [ kmeans_par   ]  a parallel version using Control.Monad.Par
--
-- Usage (sequential):
--   $ ./kmeans-par seq
--
-- Usage (Strategies):
--   $ ./kmeans-par strat 600 +RTS -N4
--
-- Usage (Par monad):
--   $ ./kmeans-par par 600 +RTS -N4
--
-- Usage (divide-and-conquer / Par monad):
--   $ ./kmeans-par divpar 7 +RTS -N4
--
-- Usage (divide-and-conquer / Eval monad):
--   $ ./kmeans-par diveval 7 +RTS -N4

import System.IO
import KMeansCore
import Data.Array
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import Debug.Trace
import Control.Parallel.Strategies as Strategies
import Control.Monad.Par as Par
import Control.DeepSeq
import System.Environment
import Data.Time.Clock
import Control.Exception
import Control.Concurrent

-- -----------------------------------------------------------------------------
-- main: read input files, time calculation

main = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read `fmap` readFile "clusters"
  let nclusters = length clusters
  args <- getArgs
  npoints <- evaluate (length points)
  t0 <- getCurrentTime
  final_clusters <- case args of
    ["seq"       ] -> kmeans_seq               nclusters points clusters
    ["strat",   n] -> kmeans_strat    (read n) nclusters points clusters
    ["par",     n] -> kmeans_par      (read n) nclusters points clusters
    ["divpar",  n] -> kmeans_div_par  (read n) nclusters points clusters npoints
    ["diveval", n] -> kmeans_div_eval (read n) nclusters points clusters npoints
    _other -> error "args"
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (sequential)

-- <<kmeans_seq
kmeans_seq :: Int -> [Vector] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do                  -- <1>
        putStrLn "giving up."
        return clusters
      loop n clusters = do
        hPrintf stdout "iteration %d\n" n
        hPutStr stdout (unlines (map show clusters))
        let clusters' = step nclusters clusters points    -- <2>
        if clusters' == clusters                          -- <3>
           then return clusters                           -- <4>
           else loop (n+1) clusters'                      -- <5>
  in
  loop 0 clusters

tooMany = 50
-- >>

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (Strategies)

-- <<kmeans_strat
kmeans_strat :: Int -> Int -> [Vector] -> [Cluster] -> IO [Cluster]
kmeans_strat numChunks nclusters points clusters =
  let
      chunks = split numChunks points                             -- <1>

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        printf "giving up."
        return clusters
      loop n clusters = do
        hPrintf stdout "iteration %d\n" n
        hPutStr stdout (unlines (map show clusters))
        let
             new_clusterss = map (step nclusters clusters) chunks -- <2>
                               `using` parList rdeepseq           -- <3>

             clusters' = reduce nclusters new_clusterss           -- <4>

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in do
  final <- loop 0 clusters
  return final
-- >>

-- <<split
split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs
-- >>

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (Par monad)

kmeans_par :: Int -> Int -> [Vector] -> [Cluster] -> IO [Cluster]
kmeans_par mappers nclusters points clusters =
  let
      chunks = split mappers points

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stdout "iteration %d\n" n
        hPutStr stdout (unlines (map show clusters))
        let
             new_clusterss = runPar $ Par.parMap (step nclusters clusters) chunks

             clusters' = reduce nclusters new_clusterss

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

-- -----------------------------------------------------------------------------
-- kmeans_div_par: Use divide-and-conquer, and the Par monad for parallellism.

kmeans_div_par :: Int -> Int -> [Vector] -> [Cluster] -> Int -> IO [Cluster]
kmeans_div_par threshold nclusters points clusters npoints =
  let
      tree = mkPointTree threshold points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let
             divconq :: Tree [Vector] -> Par [Cluster]
             divconq (Leaf points) = return $ step nclusters clusters points
             divconq (Node left right) = do
                  i1 <- spawn $ divconq left
                  i2 <- spawn $ divconq right
                  c1 <- get i1
                  c2 <- get i2
                  return $! reduce nclusters [c1,c2]

             clusters' = runPar $ divconq tree

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

data Tree a = Leaf a
            | Node (Tree a) (Tree a)


mkPointTree :: Int -> [Vector] -> Int -> Tree [Vector]
mkPointTree threshold points npoints = go 0 points npoints
 where
  go depth points npoints
   | depth >= threshold = Leaf points
   | otherwise = Node (go (depth+1) xs half)
                      (go (depth+1) ys half)
         where
                half = npoints `quot` 2
                (xs,ys) = splitAt half points

-- -----------------------------------------------------------------------------
-- kmeans_div_eval: Use divide-and-conquer, and the Eval monad for parallellism.

kmeans_div_eval :: Int -> Int -> [Vector] -> [Cluster] -> Int -> IO [Cluster]
kmeans_div_eval threshold nclusters points clusters npoints =
  let
      tree = mkPointTree threshold points npoints

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
  in
  loop 0 clusters

-- -----------------------------------------------------------------------------
-- Perform one step of the K-Means algorithm

-- <<reduce
reduce :: Int -> [[Cluster]] -> [Cluster]
reduce nclusters css =
  concatMap combine $ elems $
     accumArray (flip (:)) [] (0,nclusters) [ (clId c, c) | c <- concat css]
 where
  combine [] = []
  combine (c:cs) = [foldr combineClusters c cs]
-- >>

-- <<step
step :: Int -> [Cluster] -> [Vector] -> [Cluster]
step nclusters clusters points
   = makeNewClusters (assign nclusters clusters points)
-- >>

-- <<assign
assign :: Int -> [Cluster] -> [Vector] -> Array Int [Vector]
assign nclusters clusters points =
    accumArray (flip (:)) [] (0, nclusters-1)
       [ (clId (nearest p), p) | p <- points ]
  where
    nearest p = fst $ minimumBy (compare `on` snd)
                          [ (c, sqDistance (clCent c) p) | c <- clusters ]
-- >>

-- <<makeNewClusters
makeNewClusters :: Array Int [Vector] -> [Cluster]
makeNewClusters arr =
  filter ((>0) . clCount) $
     [ makeCluster i ps | (i,ps) <- assocs arr ]
-- >>
                        -- v. important: filter out any clusters that have
                        -- no points.  This can happen when a cluster is not
                        -- close to any points.  If we leave these in, then
                        -- the NaNs mess up all the future calculations.
