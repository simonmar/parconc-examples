import KMeansCore
import System.IO
import Data.Binary
import System.Environment

main = do
  [inf,outf] <- getArgs
  points <- readPoints inf
  encodeFile outf points


