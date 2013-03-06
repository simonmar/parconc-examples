import Control.Parallel
import Control.Parallel.Strategies (rpar, rseq, Strategy, using, rparWith)
import Control.Exception
import Text.Printf
import System.Environment

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

main = print pair
 where
  pair =
-- <<pair
   (fib 35, fib 36) `using` parPair rseq rseq
-- >>

-- <<evalPair
evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair sa sb (a,b) = do
  a' <- sa a
  b' <- sb b
  return (a',b')
-- >>

-- <<parPair
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
-- >>
