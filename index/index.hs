import Control.Monad.Par
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as LL
import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

import System.Environment
import System.Exit
import System.IO
import Data.Array
import Data.Char
import Control.Monad

-- A document index and search program.  Use it like this:
--
-- $ ./index docs/*
-- search: <enter search term here>
-- docs/file1
-- docs/file2
-- docs/file3
--
--

type Word = B.ByteString

-- Documents are numbered by the order they appear on the command line
type DocSet = IntSet

-- A DocIndex maps a word to the set of documents that contain the word
type DocIndex = Map Word DocSet

joinIndices :: [DocIndex] -> DocIndex
joinIndices = foldr (Map.unionWith Set.union) Map.empty

mkIndex :: Int -> L.ByteString -> DocIndex
mkIndex i s
  = Map.fromListWith Set.union [ (B.concat (L.toChunks w), Set.singleton i)
                               | w <- ws ]
  where ws = L.splitWith (not . isAlphaNum) s

search :: DocIndex -> [Word] -> DocSet
search index words = foldr1 Set.intersection (map lookup words)
  where lookup w = Map.findWithDefault Set.empty w index

-- -----------------------------------------------------------------------------

main = do
  hSetBuffering stdout NoBuffering
  fs <- getArgs

  -- Step 1: build the index
  ss <- mapM L.readFile fs
  let
      -- indices is a separate index for each (numbered) document
      indices :: [DocIndex]
      indices = zipWith mkIndex [0..] ss

      -- union the indices together
      index = joinIndices indices

      -- array mapping doc number back to filename
      arr = listArray (0,length fs - 1) fs

  -- Step 2: perform search
  forever $ do
    putStr "search (^D to end): "
    eof <- isEOF
    when eof $ exitWith ExitSuccess
    s <- B.getLine
    putStr "wait... "

    let result :: DocSet  -- set of docs containing the words in the term
        result = search index (B.words s)

        -- map the result back to filenames
        files = map (arr !) (Set.toList result)

    putStrLn ("\n" ++ unlines files)
