import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Control.DeepSeq
import Data.Maybe

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    evaluate (length grids)
    print $ length $ filter isJust $ runEval $ parMap solve grids

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
