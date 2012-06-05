import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    print $ length $ filter isJust $ runEval $ do
       as' <- rpar (force (map solve as))
       bs' <- rpar (force (map solve bs))
       rseq as'
       rseq bs'
       return (as' ++ bs')
