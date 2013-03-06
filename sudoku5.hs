import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
-- <<solutions
  let solutions = map solve puzzles `using` parList rseq
-- >>

  print (length (filter isJust solutions))
