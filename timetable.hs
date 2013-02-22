{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Data.Text (Text)
import System.Random
import System.Environment
import Debug.Trace
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Class = Class Int
  deriving (Eq,Ord)

instance Show Class where
  show (Class t) = show t

data Student = Student
  { name    :: Text
  , classes :: [Class]
  }
  deriving (Show)

type TimeTable = [[Class]]

selects xs [] = []
selects xs (y:ys) = (y,xs++ys) : selects (y:xs) ys

solve :: [Student] -> [Class] -> Int -> Int -> [TimeTable]
solve students all_classes maxDay maxSlot =
  trace (show clashesWith) $
  generate all_classes all_classes 0 [] 0 []
 where
  clashesWith :: Map Class (Set Class)
  clashesWith = Map.fromListWith Set.union
     [ (c, Set.fromList cs)
     | s <- students, (c, cs) <- selects [] (classes s) ]

  generate this_slot cs slot slots day days
--    | trace (show slot ++ " " ++ show day ++ " " ++ show this_slot) False = undefined
    | slot == maxSlot = [ slots ]
    | day == maxDay   = generate cs cs (slot+1) (days:slots) 0 []
    | otherwise =
      concat [ generate (filter (`notElem` Set.toList (Map.findWithDefault Set.empty c clashesWith)) ts') (filter (/= c) cs) slot slots (day+1) (c:days)
             | (c,ts') <- selects [] this_slot
             ]

   -- Keep a list of classes we can put in this slot
   -- if it is empty, backtrack
   -- For each class,
   -- Pick one of their classes that is in our list of classes
   --


-- ----------------------------------------------------------------------------

bench :: Int -> Int -> Int -> Int -> Int -> StdGen -> ([Student],[Class],[TimeTable])
bench nslots ndays nclasses nstudents c_per_s gen =
  trace (show classes) $
  (students,classes,solve students classes ndays nslots)
 where
  total_classes = nslots * ndays

  classes = map Class [1..total_classes]
  students = mkstudents nstudents gen

  mkstudents :: Int -> StdGen -> [Student]
  mkstudents 0 g = []
  mkstudents n g = Student (Text.pack ('S':show n)) (take c_per_s cs) : rest
        where
          (g1,g2) = split g
          rest = mkstudents (n-1) g2
          cs = nub [ classes !! n | n <- randomRs (0,nclasses-1) g ]

main = do
   [ a, b, c, d, e ] <- fmap (fmap read) getArgs
   let g = mkStdGen 1001
   let (ss,cs,ts) = bench a b c d e g
   print ss
   print (head ts)
--   [ a, b ] <- fmap (fmap read) getArgs
--   print (head (test2 a b))

test = solve testStudents cs 2 2
 where
   cs@[ca,cb,cc,cd] = map Class [1..4]

   testStudents =
    [ Student "P" [ca,cb]
    , Student "Q" [cb,cc]
    , Student "R" [cc,cd]
    ]

test2 n m = solve testStudents cs m n
 where
   cs = map Class [1 .. (n * m)]

   testStudents =
    [ Student "1"  (take n cs)
    ]
