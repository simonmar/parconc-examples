{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
--
-- With three versions:
--   [ kmeans_seq   ]  a sequential version
--   [ kmeans_strat ]  a parallel version using Control.Parallel.Strategies
--   [ kmeans_par   ]  a parallel version using Control.Monad.Par
--
-- Usage (sequential):
--   $ ./kmeans seq
--
-- Usage (Strategies):
--   $ ./kmeans strat 600 +RTS -N4
--
-- Usage (Par monad):
--   $ ./kmeans par 600 +RTS -N4
--
-- Usage (divide-and-conquer / Par monad):
--   $ ./kmeans divpar 7 +RTS -N4
--
-- Usage (divide-and-conquer / Eval monad):
--   $ ./kmeans diveval 7 +RTS -N4

import System.IO
import KMeansCore
import Data.Array
import Data.Array.Unsafe as Unsafe
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
import Control.Monad.ST
import Data.Array.ST
import System.Mem
import Data.Maybe

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MVector

-- -----------------------------------------------------------------------------
-- main: read input files, time calculation

main = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read `fmap` readFile "clusters"
  let nclusters = length clusters
  args <- getArgs
  npoints <- evaluate (length points)
  performGC
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
kmeans_seq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do                  -- <1>
        putStrLn "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = step nclusters clusters points    -- <2>
        if clusters' == clusters                          -- <3>
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

tooMany = 80
-- >>

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (Strategies)

-- <<kmeans_strat
kmeans_strat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_strat numChunks nclusters points clusters =
  let
      chunks = split numChunks points                            -- <1>

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        printf "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = parSteps_strat nclusters clusters chunks -- <2>
        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters
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

kmeans_par :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_par mappers nclusters points clusters =
  let
      chunks = split mappers points

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let
             clusters' = steps_par nclusters clusters chunks

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

-- -----------------------------------------------------------------------------
-- kmeans_div_par: Use divide-and-conquer, and the Par monad for parallellism.

kmeans_div_par :: Int -> Int -> [Point] -> [Cluster] -> Int -> IO [Cluster]
kmeans_div_par threshold nclusters points clusters npoints =
  let
      tree = mkPointTree threshold points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let
             divconq :: Tree [Point] -> Par (Vector PointSum)
             divconq (Leaf points) = return $ assign nclusters clusters points
             divconq (Node left right) = do
                  i1 <- spawn $ divconq left
                  i2 <- spawn $ divconq right
                  c1 <- get i1
                  c2 <- get i2
                  return $! combine c1 c2

             clusters' = makeNewClusters $ runPar $ divconq tree

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

data Tree a = Leaf a
            | Node (Tree a) (Tree a)


mkPointTree :: Int -> [Point] -> Int -> Tree [Point]
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

kmeans_div_eval :: Int -> Int -> [Point] -> [Cluster] -> Int -> IO [Cluster]
kmeans_div_eval threshold nclusters points clusters npoints =
  let
      tree = mkPointTree threshold points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let
             divconq :: Tree [Point] -> Vector PointSum
             divconq (Leaf points) = assign nclusters clusters points
             divconq (Node left right) = runEval $ do
                  c1 <- rpar $ divconq left
                  c2 <- rpar $ divconq right
                  rdeepseq c1
                  rdeepseq c2
                  return $! combine c1 c2

             clusters' = makeNewClusters $ divconq tree

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

-- -----------------------------------------------------------------------------
-- Perform one step of the K-Means algorithm

-- <<step
step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points
   = makeNewClusters (assign nclusters clusters points)
-- >>

-- <<assign
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
    vec <- MVector.replicate nclusters (PointSum 0 0 0)
    let
        addpoint p = do
          let c = nearest p; cid = clId c
          ps <- MVector.read vec cid
          MVector.write vec cid $! addToPointSum ps p

    mapM_ addpoint points
    return vec
 where
  nearest p = fst $ minimumBy (compare `on` snd)
                        [ (c, sqDistance (clCent c) p) | c <- clusters ]
-- >>

data PointSum = PointSum {-# UNPACK #-} !Int {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance NFData PointSum where
  rnf (PointSum count xs ys) = () -- all fields are strict

-- <<addToPointSum
addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y)
  = PointSum (count+1) (xs + x) (ys + y)
-- >>

-- <<pointSumToCluster
pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
  Cluster { clId    = i
          , clCent  = Point (xs / fromIntegral count) (ys / fromIntegral count)
          }
-- >>

-- <<addPointSums
addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum c1 x1 y1) (PointSum c2 x2 y2)
  = PointSum (c1+c2) (x1+x2) (y1+y2)
-- >>

-- <<combine
combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums
-- >>

-- <<parSteps_strat
parSteps_strat :: Int -> [Cluster] -> [[Point]] -> [Cluster]
parSteps_strat nclusters clusters pointss
  = makeNewClusters $
      foldr1 combine $
          (map (assign nclusters clusters) pointss
            `using` parList rseq)
-- >>

steps_par :: Int -> [Cluster] -> [[Point]] -> [Cluster]
steps_par nclusters clusters pointss
  = makeNewClusters $
      foldl1' combine $
          (runPar $ Par.parMap (assign nclusters clusters) pointss)

-- <<makeNewClusters
makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
  | (i,ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec)
  , count > 0
  ]
-- >>
                        -- v. important: filter out any clusters that have
                        -- no points.  This can happen when a cluster is not
                        -- close to any points.  If we leave these in, then
                        -- the NaNs mess up all the future calculations.
