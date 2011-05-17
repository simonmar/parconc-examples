--
-- Sudoku solver using constraint propagation.  The algorithm is by
-- Peter Norvig http://norvig.com/sudoku.html; the Haskell
-- implementation is by Manu and Daniel Fischer, and can be found on
-- the Haskell Wiki http://www.haskell.org/haskellwiki/Sudoku
--
-- The Haskell wiki license applies to this code:
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- this work (the "Work"), to deal in the Work without restriction,
-- including without limitation the rights to use, copy, modify, merge,
-- publish, distribute, sublicense, and/or sell copies of the Work, and
-- to permit persons to whom the Work is furnished to do so.
-- 
-- THE WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE WORK OR THE USE OR OTHER DEALINGS IN THE WORK.

module Sudoku (solve, printGrid) where

import Data.List hiding (lookup)
import Data.Array
import Control.Monad
import Data.Maybe

-- Types
type Digit  = Char
type Square = (Char,Char)
type Unit   = [Square]
 
-- We represent our grid as an array
type Grid = Array Square [Digit]
 
 
-- Setting Up the Problem
rows = "ABCDEFGHI"
cols = "123456789"
digits = "123456789"
box = (('A','1'),('I','9'))
 
cross :: String -> String -> [Square]
cross rows cols = [ (r,c) | r <- rows, c <- cols ]
 
squares :: [Square]
squares = cross rows cols  -- [('A','1'),('A','2'),('A','3'),...]
 
peers :: Array Square [Square]
peers = array box [(s, set (units!s)) | s <- squares ]
      where
        set = nub . concat
 
unitlist :: [Unit]
unitlist = [ cross rows [c] | c <- cols ] ++
            [ cross [r] cols | r <- rows ] ++
            [ cross rs cs | rs <- ["ABC","DEF","GHI"], 
                            cs <- ["123","456","789"]]
 
-- this could still be done more efficiently, but what the heck...
units :: Array Square [Unit]
units = array box [(s, [filter (/= s) u | u <- unitlist, s `elem` u ]) | 
                    s <- squares]
 
 
allPossibilities :: Grid
allPossibilities = array box [ (s,digits) | s <- squares ]
 
-- Parsing a grid into an Array
parsegrid     :: String -> Maybe Grid
parsegrid g    = do regularGrid g
                    foldM assign allPossibilities (zip squares g)
 
   where  regularGrid   :: String -> Maybe String
          regularGrid g  = if all (`elem` "0.-123456789") g
                              then Just g
                              else Nothing
 
-- Propagating Constraints
assign        :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if d `elem` digits
                 -- check that we are assigning a digit and not a '.'
                  then do
                    let ds = g ! s
                        toDump = delete d ds
                    foldM eliminate g (zip (repeat s) toDump)
                  else return g
 
eliminate     ::  Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) = 
  let cell = g ! s in
  if d `notElem` cell then return g -- already eliminated
  -- else d is deleted from s' values
    else do let newCell = delete d cell
                newV = g // [(s,newCell)]
            newV2 <- case newCell of
            -- contradiction : Nothing terminates the computation
                 []   -> Nothing
            -- if there is only one value left in s, remove it from peers
                 [d'] -> do let peersOfS = peers ! s
                            foldM eliminate newV (zip peersOfS (repeat d'))
            -- else : return the new grid
                 _    -> return newV
            -- Now check the places where d appears in the peers of s
            foldM (locate d) newV2 (units ! s)
 
locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (g !)) u of
                []  -> Nothing
                [s] -> assign g (s,d)
                _   -> return g
 
-- Search
search :: Grid -> Maybe Grid
search g = 
  case [(l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1] of
            [] -> return g
            ls -> do let (_,(s,ds)) = minimum ls
                     msum [assign g (s,d) >>= search | d <- ds]
 
solve :: String -> Maybe Grid
solve str = do
    grd <- parsegrid str
    search grd
 
-- Display solved grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString
 
gridToString :: Grid -> String
gridToString g =
  let l0 = elems g
      -- [("1537"),("4"),...]   
      l1 = (map (\s -> " " ++ s ++ " ")) l0
      -- ["1 "," 2 ",...] 
      l2 = (map concat . sublist 3) l1
      -- ["1  2  3 "," 4  5  6 ", ...]
      l3 = (sublist 3) l2
      -- [["1  2  3 "," 4  5  6 "," 7  8  9 "],...] 
      l4 = (map (concat . intersperse "|")) l3
      -- ["1  2  3 | 4  5  6 | 7  8  9 ",...]
      l5 = (concat . intersperse [line] . sublist 3) l4
  in unlines l5 
     where sublist n [] = []
           sublist n xs = ys : sublist n zs
             where (ys,zs) = splitAt n xs
           line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
           hyphens = replicate 9 '-'
