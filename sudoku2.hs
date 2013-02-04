import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file

      (as,bs) = splitAt (length puzzles `div` 2) puzzles -- <1>

      solutions = runEval $ do
                    as' <- rpar (force (map solve as))   -- <2>
                    bs' <- rpar (force (map solve bs))   -- <2>
                    rseq as'                             -- <3>
                    rseq bs'                             -- <3>
                    return (as' ++ bs')                  -- <4>

  print (length (filter isJust solutions))
-- >>

