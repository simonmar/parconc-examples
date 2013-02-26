--
-- Derived from a program believed to be originally written by John
-- Launchbury, and incorporating the RSA algorithm which is in the
-- public domain.
--

import System.Environment
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import ByteStringCompat
import Control.Monad.Par.Scheds.Trace

import Stream

main = do
  [f] <- getArgs
  text <- case f of
            "-" -> B.getContents
            _   -> B.readFile f
  B.putStr (pipeline n e d text)

-- example keys, created by makeKey below
n, d, e :: Integer
(n,d,e) = (3539517541822645630044332546732747854710141643130106075585179940882036712515975698104695392573887034788933523673604280427152984392565826058380509963039612419361429882234327760449752708861159361414595229,121492527803044541056704751360974487724009957507650761043424679483464778334890045929773805597614290949,216244483337223224019000724904989828660716358310562600433314577442746058361727768326718965949745599136958260211917551718034992348233259083876505235987999070191048638795502931877693189179113255689722281)


-- <<encrypt
encrypt :: Integer -> Integer -> Stream ByteString -> Par (Stream ByteString)
encrypt n e s = streamMap (B.pack . show . power e n . code) s

decrypt :: Integer -> Integer -> Stream ByteString -> Par (Stream ByteString)
decrypt n d s = streamMap (B.pack . decode . power d n . integer) s
-- >>

integer :: ByteString -> Integer
integer b | Just (i,_) <- B.readInteger b = i

-- <<pipeline
pipeline :: Integer -> Integer -> Integer -> ByteString -> ByteString
pipeline n e d b = runPar $ do
  s0 <- streamFromList (chunk (size n) b)
  s1 <- encrypt n e s0
  s2 <- decrypt n d s1
  xs <- streamFold (\x y -> (y : x)) [] s2
  return (B.unlines (reverse xs))
-- >>

integers :: [ByteString] -> [Integer]
integers bs = [ i | Just (i,_) <- map B.readInteger bs ]

-------- Converting between Strings and Integers -----------

code :: ByteString -> Integer
code = B.foldl' accum 0
  where accum x y = (128 * x) + fromIntegral (fromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
   where expand 0 = []
         expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

chunk :: Int -> ByteString -> [ByteString]
chunk n xs | B.null xs = []
chunk n xs = as : chunk n bs
  where (as,bs) = B.splitAt (fromIntegral n) xs

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100	-- log_128 10 = 0.4745

------- Constructing keys -------------------------

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys r s = (p*q, d, invert ((p-1)*(q-1)) d)
   where   p = nextPrime r
           q = nextPrime s
	   d = nextPrime (p+q+1)

nextPrime :: Integer -> Integer
nextPrime a = head (filter prime [odd,odd+2..])
  where  odd | even a = a+1
             | True   = a
         prime p = and [power (p-1) p x == 1 | x <- [3,5,7]]

invert :: Integer -> Integer -> Integer
invert n a = if e<0 then e+n else e
  where  e=iter n 0 a 1

iter :: Integer -> Integer -> Integer -> Integer -> Integer
iter g v 0 w = v
iter g v h w = iter h w (g `mod` h) (v - (g `div` h)*w)

------- Fast exponentiation, mod m -----------------

power :: Integer -> Integer -> Integer -> Integer
power 0 m x          = 1
power n m x | even n = sqr (power (n `div` 2) m x) `mod` m
	    | True   = (x * power (n-1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x


