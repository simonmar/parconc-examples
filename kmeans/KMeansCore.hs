--
-- Adapted from the K-Means example in the remote-0.1.1 package,
--   (c) Jeff Epstein <jepst79@gmail.com>
--

{-# LANGUAGE DeriveDataTypeable #-}
module KMeansCore where

import Data.List
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Control.DeepSeq

-- -----------------------------------------------------------------------------
-- Vectors

data Vector = Vector {-#UNPACK#-}!Double {-#UNPACK#-}!Double
    deriving (Show,Read,Eq)

-- <<vector-ops
zeroVector :: Vector
zeroVector = Vector 0 0

addVector :: Vector -> Vector -> Vector
addVector (Vector a b) (Vector c d) = Vector (a+c) (b+d)

sqDistance :: Vector -> Vector -> Double
sqDistance (Vector x1 y1) (Vector x2 y2) = ((x1-x2)^2) + ((y1-y2)^2)
-- >>

instance Binary Vector where
  put (Vector a b) = put a >> put b
  get = do a <- get; b <- get; return (Vector a b)

readPoints :: FilePath -> IO [Vector]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Vector (read (B.unpack sx)) (read (B.unpack sy))
               | (sx:sy:_) <- ls ]
  --
  return points

-----------------------------------------------------------------------------
-- Clusters

data Cluster
  = Cluster { clId    :: {-# UNPACK #-} !Int
            , clCount :: {-# UNPACK #-} !Int
            , clSum   :: {-# UNPACK #-} !Vector
            , clCent  :: {-# UNPACK #-} !Vector
            }
  deriving (Show,Read,Eq)

instance NFData Cluster  -- default is ok, all the fields are strict

-- <<makeCluster
makeCluster :: Int -> [Vector] -> Cluster
makeCluster clid vecs =
  Cluster { clId    = clid
          , clCount = count
          , clSum   = vecsum
          , clCent  = Vector (a / fromIntegral count) (b / fromIntegral count)
          }
 where
  vecsum@(Vector a b) = foldl' addVector zeroVector vecs
  count = length vecs
-- >>

-- <<combineClusters
combineClusters c1 c2 =
  Cluster { clId    = clId c1
          , clCount = count
          , clSum   = vecsum
          , clCent  = Vector (a / fromIntegral count) (b / fromIntegral count)
          }
 where
  count = clCount c1 + clCount c2
  vecsum@(Vector a b) = addVector (clSum c1) (clSum c2)
-- >>
