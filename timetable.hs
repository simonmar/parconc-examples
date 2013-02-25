{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Data.Text (Text)
import System.Random
import System.Environment
import Debug.Trace
import Data.List
import Control.Monad.Par

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

solve :: ( state -> Bool )                           -- finished?
      -> ( solution -> state -> [(solution,state)] ) -- extend a solution
      -> solution                                    -- initial solution
      -> state                                       -- initial state
      -> [ solution ]

solve finished extend emptysoln initstate
  = generate emptysoln initstate
  where
    generate soln state
       | finished state = [ soln ]
       | otherwise      = concat [ generate soln' state'
                                 | (soln', state') <- extend soln state ]


-- ----------------------------------------------------------------------------

type State    = ([Talk], [Talk], Int, Int, [Talk])
type Solution = [[Talk]]

schedule persons all_talks maxTrack maxSlot =
  solve finished extend emptysoln initstate
 where
  emptysoln = []

  initstate = (all_talks, all_talks, 0, 0, [])

  finished (this_slot, ts, slot, track, tracks) = slot == maxSlot

  clashesWith :: Map Talk [Talk]
  clashesWith = Map.fromListWith (\xs ys -> filter (`notElem` ys) xs ++ ys)
     [ (c, ts)
     | s <- persons, (c, ts) <- selects [] (talks s) ]

  extend slots (this_slot, ts, slot, track, tracks)
     | track == maxTrack = (tracks : slots , (ts, ts, slot, 0, []))
     | otherwise =
         [ (slots, (this_slot', (filter (/= c) ts), slot, track+1, c:tracks))
         | (c,ts') <- selects [] this_slot
         , let clashes = Map.findWithDefault [] c clashesWith
         , let this_slot' = filter (`notElem` clashes) ts'
         ]

-- ----------------------------------------------------------------------------

bench :: Int -> Int -> Int -> Int -> Int -> StdGen -> ([Person],[Talk],[TimeTable])
bench nslots ntracks ntalks npersons c_per_s gen =
  (persons,talks, runPar $ solve persons talks ntracks nslots)
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

test = solve testPersons cs 2 2
 where
   cs@[ca,cb,cc,cd] = map Talk [1..4]

   testPersons =
    [ Person "P" [ca,cb]
    , Person "Q" [cb,cc]
    , Person "R" [cc,cd]
    ]

test2 n m = solve testPersons cs m n
 where
   cs = map Talk [1 .. (n * m)]

   testPersons =
    [ Person "1"  (take n cs)
    ]
