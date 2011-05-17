import KMeansCommon
import Data.Random.Normal
import System.Random
import System.IO
import Data.Array
import System.Environment
import Control.Monad
import Data.List
import Data.Binary

minX, maxX, minY, maxY, minSD, maxSD :: Double
minX = -0
maxX = 10
minY = -10
maxY = 10
minSD = 0.5
maxSD = 1.5

main = do
    [n, minp, maxp] <- fmap (fmap read) getArgs

    gen <- getStdGen
    nps <- replicateM n (randomRIO (minp, maxp))
    xs  <- replicateM n (randomRIO (minX, maxY))
    ys  <- replicateM n (randomRIO (minX, maxY))
    sds <- replicateM n (randomRIO (minSD, maxSD))

    let params = zip5 nps xs ys sds sds

    -- first generate a set of points for each set of sample parameters
    ss <- mapM (\(a,b,c,d,e) -> generate2DSamples a b c d e) params
    let points = concat ss

    -- dump all the points into the file "points"
    hsamp <- openFile "points" WriteMode
    mapM_ (printPoint hsamp) points
    hClose hsamp

    encodeFile "points.bin" points

    -- generate the initial clusters by assigning each point to random
    -- cluster.
    gen <- getStdGen
    let
        rand_clusters = randomRs (0,n-1) gen :: [Int]
        arr = accumArray (flip (:)) [] (0,n-1) $
                zip rand_clusters points
        clusters = map (uncurry makeCluster) (assocs arr)
    writeFile "clusters" (show clusters)

    -- so we can tell what the answer should be:
    writeFile "params" (show params)


printPoint :: Handle -> Vector -> IO ()
printPoint h (Vector x y) = do
  hPutStr h (show x)
  hPutChar h ' '
  hPutStr h (show y)
  hPutChar h '\n'

generate2DSamples :: Int                 -- number of samples to generate
                  -> Double -> Double    -- X and Y of the mean
                  -> Double -> Double    -- X and Y standard deviations
                  -> IO [Vector]

generate2DSamples n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return (zipWith Vector (take n xsamples) ysamples)
