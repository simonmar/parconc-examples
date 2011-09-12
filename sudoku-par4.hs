import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Monad.Par
import Control.DeepSeq

main :: IO ()
main = do
    [f,n] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (runPar $ parMapChunk (read n) solve grids)))

parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = fmap concat $ parMap (map f) (chunk n xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

-- No chunks (sudoku-par3):
--   Total time   43.71s  ( 43.73s elapsed)
-- chunk 100:
--   Total time   44.43s  ( 44.44s elapsed)
-- 
-- No chunks, -N8:
--   Total time   67.73s  (  8.38s elapsed)
--   (5.21x)
-- chunk 10, -N8:
--   Total time   61.62s  (  7.74s elapsed)
--   (5.64x)
-- chunk 100, -N8:
--   Total time   60.81s  (  7.73s elapsed)
--   (5.65x)
-- chunk 1000, -N8:
--   Total time   61.74s  (  7.88s elapsed)
--   (5.54x)
