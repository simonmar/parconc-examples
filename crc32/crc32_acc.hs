import Data.Word
import Data.Bits
import Data.Char
import Debug.Trace

import Data.Array.Accelerate hiding (fromIntegral, shiftR, map, zipWith, unzip, replicate)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter

import CRC32

crc32_one :: Acc (Vector Word32) -> Exp Word32 -> Exp Word8 -> Exp Word32
crc32_one tab crc char
  = (char ==* 0) ?
       ( crc
       , (tab ! index1 (A.fromIntegral (A.fromIntegral crc `xor` char)))
             `xor`
          crc `A.shiftR` 8
       )


crcAll :: [String] -> Acc (Vector Word32)
crcAll words = all width init_crcs words
  where
    n = length words

    width = maximum (0 : map length words)

    table :: Acc (Vector Word32)
    table = use (fromList (Z:.256) crc32_tab)

    init_crcs :: Acc (Vector Word32)
    init_crcs = fill (index1 (constant n)) (constant 0xffffffff)

    myHead :: String -> (Word8, String)
    myHead []     = (0, [])
    myHead (c:cs) = (fromIntegral (ord c), cs)

    one_iter :: Acc (Vector Word32) -> Acc (Vector Word8)
             -> Acc (Vector Word32)
    one_iter crcs chars = A.zipWith (crc32_one table) crcs chars

    all :: Int -> Acc (Vector Word32) -> [String] -> Acc (Vector Word32)
    all 0 crcs _     = crcs
    all x crcs words = all (x-1) (one_iter crcs chars) tails
      where
      chars = use (fromList (Z:.n) heads)
      (heads, tails) = unzip (map myHead words)

find :: Acc (Vector Word32) -> Acc (Scalar Int)
find arr = A.fold A.max (constant 0) (A.zipWith check arr ixs)
  where check :: Exp Word32 -> Exp Int -> Exp Int
        check x ix = x ==* password_hash ? ( ix, 0 )

        ixs = generate (shape arr) unindex1

password_hash = constant 0xb4967c42 :: Exp Word32

main = do
  s <- readFile "/usr/share/dict/american-english"
  let ls = lines s
  let [r] = toList $ run $ find $ crcAll ls
  print (ls !! r)
