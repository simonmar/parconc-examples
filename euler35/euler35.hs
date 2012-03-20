import Data.List (tails)
 
import Control.Monad.Par
import Stream

primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where
        factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
                        | otherwise      = factor m ps
 
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = case (primeFactors n) of
                (_:_:_)   -> False
                _         -> True
 
permutations :: Integer -> [Integer]
permutations n = take l $ map (read . take l) $ tails $ take (2*l -1) $ cycle s
    where
        s = show n
        l = length s
 
circular :: Integer -> Bool
circular x = all isPrime (permutations x)

-- Original sequential version:
-- problem_35 :: Int
-- problem_35 = length $ filter circular $ takeWhile (<1000000) primes

main :: IO ()
main = print $ runPar $ do
     s1 <- streamFromList (takeWhile (<1000000) primes)
     s2 <- streamFilter circular s1
     streamFold (\a _ -> a + 1) 0 s2
