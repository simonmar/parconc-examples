import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

main :: IO ()
main = do
    [f] <- getArgs
    grids <- lines <$> readFile f
    print (length (filter isJust (map solve grids)))
