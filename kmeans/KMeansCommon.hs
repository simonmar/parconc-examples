{-# LANGUAGE DeriveDataTypeable #-}
module KMeansCommon where

import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Control.DeepSeq

data Vector = Vector {-#UNPACK#-}!Double {-#UNPACK#-}!Double deriving (Show,Read,Typeable,Data,Eq)

instance Binary Vector where put (Vector a b) = put a>>put b
                             get = do a<-get
                                      b<-get
                                      return $ Vector a b

data Cluster = Cluster
               {
                  clId    :: {-#UNPACK#-}!Int,
                  clCount :: {-#UNPACK#-}!Int,
                  clSum   :: {-#UNPACK#-}!Vector,
                  clCent  :: {-#UNPACK#-}!Vector
               } deriving (Show,Read,Typeable,Data,Eq)

instance NFData Cluster  -- default should be fine

sqDistance :: Vector -> Vector -> Double
sqDistance (Vector x1 y1) (Vector x2 y2) = ((x1-x2)^2) + ((y1-y2)^2)

makeCluster :: Int -> [Vector] -> Cluster
makeCluster clid vecs
   = Cluster { clId = clid,
               clCount = count,
               clSum = vecsum,
               clCent = centre
             }
   where vecsum@(Vector a b)  = foldl' addVector zeroVector vecs
         centre = Vector (a / fromIntegral count) (b / fromIntegral count)
         count = length vecs

combineClusters c1 c2 =
  Cluster {clId = clId c1,
           clCount = count,
           clSum = vecsum,
           clCent = Vector (a / fromIntegral count) (b / fromIntegral count)}
  where count = clCount c1 + clCount c2
        vecsum@(Vector a b)  = addVector (clSum c1) (clSum c2)

addVector (Vector a b) (Vector c d) = Vector (a+c) (b+d)
zeroVector = Vector 0 0

getPoints :: FilePath -> IO [Vector]
getPoints fp = do c <- readFile fp
                  return $ read c

getClusters :: FilePath -> IO [Cluster]
getClusters fp = do c <- readFile fp
                    return $ read c

readPoints :: FilePath -> IO [Vector]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Vector (read (B.unpack sx)) (read (B.unpack sy))
               | (sx:sy:_) <- ls ]
  --
  return points
