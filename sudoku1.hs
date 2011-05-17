import Sudoku
import Control.Exception
import System.Environment

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    mapM_ (evaluate . solve) grids

--    let (as,bs) = splitAt (length grids `div` 2) grids
--
--    print $ runEval $ do
--       asols <- rpar (map solve as `using` rdeepseq)
--       bsols <- rpar (map solve bs `using` rdeepseq)
--       rseq asols
--       rseq bsols
--       return ()

--    let solutions = map solve grids `using` parList rseq
--    mapM_ evaluate solutions

