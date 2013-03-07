{-# LANGUAGE FlexibleContexts, BangPatterns #-}

import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import System.Environment
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word

-- <<main
main :: IO ()
main = do
    [n, f1,f2] <- getArgs
    runIL $ do
      (RGB v) <- readImage f1
      rotated <- computeP $ rotate (read n) v :: IL (Array F DIM3 Word8)
      writeImage f2 (RGB rotated)
-- >>

-- <<rotate
rotate :: Double -> Array F DIM3 Word8 -> Array D DIM3 Word8
rotate deg g = fromFunction (Z :. y :. x :. k) f
    where
        sh@(Z :. y :. x :. k)   = extent g

        !theta = pi/180 * deg

        !st = sin theta
        !ct = cos theta

        !fy2 = fromIntegral y / 2 :: Double
        !fx2 = fromIntegral x / 2 :: Double

        f (Z :. i :. j :. k)
          | inShape sh new = g ! new
          | otherwise      = 0 -- black
          where
            new = Z :. i' :. j' :. k

            fi = fromIntegral i - fy2
            fj = fromIntegral j - fx2

            i' = round (st * fj + ct * fi + fy2)
            j' = round (ct * fj - st * fi + fx2)
-- >>

