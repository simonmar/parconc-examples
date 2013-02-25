{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Data.Text (Text)
import System.Random
import System.Environment
import Debug.Trace
import Data.List
import Control.Monad.Par
import Control.DeepSeq

import Data.Map (Map)
import qualified Data.Map as Map

newtype Talk = Talk Int
  deriving (Eq,Ord)

instance NFData Talk

instance Show Talk where
  show (Talk t) = show t

data Person = Person
  { name    :: Text
  , talks :: [Talk]
  }
  deriving (Show)

type TimeTable = [[Talk]]

selects xs [] = []
selects xs (y:ys) = (y,xs++ys) : selects (y:xs) ys


-- ----------------------------------------------------------------------------

solve :: ( partial -> Maybe solution )   -- finished?
      -> ( partial -> [ partial ] )      -- refine a solution
      -> partial                         -- initial solution
      -> [solution]

solve finished refine emptysoln
  = generate emptysoln
  where
    generate partial
       | Just soln <- finished partial = [soln]
       | otherwise  = concat (map generate (refine partial))

parsolve :: NFData solution
      => Int
      -> ( partial -> Maybe solution )   -- finished?
      -> ( partial -> [ partial ] )      -- refine a solution
      -> partial                         -- initial solution
      -> [solution]

parsolve maxdepth finished refine emptysoln
  = runPar $ generate 0 emptysoln
  where
    generate d partial | d >= maxdepth = return (solve finished refine partial)
    generate d partial
       | Just soln <- finished partial = return [soln]
       | otherwise  = do
           is <- mapM (spawn . generate (d+1)) (refine partial)
           solnss <- mapM get is
           return (concat solnss)

-- ----------------------------------------------------------------------------

type Partial  = ( [Talk]    -- talks that can be allocated in the current slot
                , [Talk]    -- all talks remaining to allocate
                , Int       -- current slot number
                , [[Talk]]  -- completed slots so far
                , Int       -- current track number
                , [Talk]    -- talks in the current slot so far
                )
type Solution = [[Talk]]

schedule persons all_talks maxTrack maxSlot =
  parsolve 3 finished refine emptysoln
 where
  emptysoln = (all_talks, all_talks, 0, [], 0, [])

  finished (this_slot, ts, slot, slots, track, tracks)
     | slot == maxSlot = Just slots
     | otherwise       = Nothing

  clashesWith :: Map Talk [Talk]
  clashesWith = Map.fromListWith (\xs ys -> filter (`notElem` ys) xs ++ ys)
     [ (t, ts)
     | s <- persons
     , (t, ts) <- selects [] (talks s) ]

  refine (this_slot, ts, slot, slots, track, tracks)
     | track == maxTrack = [(ts, ts, slot+1, tracks : slots, 0, [])]
     | otherwise =
         [ (this_slot', filter (/= c) ts, slot, slots, track+1, c:tracks)
         | (c,ts') <- selects [] this_slot
         , let clashes = Map.findWithDefault [] c clashesWith
         , let this_slot' = filter (`notElem` clashes) ts'
         ]

-- ----------------------------------------------------------------------------

bench :: Int -> Int -> Int -> Int -> Int -> StdGen -> ([Person],[Talk],[TimeTable])
bench nslots ntracks ntalks npersons c_per_s gen =
  (persons,talks, schedule persons talks ntracks nslots)
 where
  total_talks = nslots * ntracks

  talks = map Talk [1..total_talks]
  persons = mkpersons npersons gen

  mkpersons :: Int -> StdGen -> [Person]
  mkpersons 0 g = []
  mkpersons n g = Person (Text.pack ('S':show n)) (take c_per_s cs) : rest
        where
          (g1,g2) = split g
          rest = mkpersons (n-1) g2
          cs = nub [ talks !! n | n <- randomRs (0,ntalks-1) g ]

main = do
   [ a, b, c, d, e ] <- fmap (fmap read) getArgs
   let g = mkStdGen 1001
   let (ss,cs,ts) = bench a b c d e g
   print ss
   print (length ts)
--   [ a, b ] <- fmap (fmap read) getArgs
--   print (head (test2 a b))

test = schedule testPersons cs 2 2
 where
   cs@[ca,cb,cc,cd] = map Talk [1..4]

   testPersons =
    [ Person "P" [ca,cb]
    , Person "Q" [cb,cc]
    , Person "R" [cc,cd]
    ]

test2 n m = schedule testPersons cs m n
 where
   cs = map Talk [1 .. (n * m)]

   testPersons =
    [ Person "1"  (take n cs)
    ]
