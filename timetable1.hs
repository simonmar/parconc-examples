{-# LANGUAGE OverloadedStrings #-}

import System.Random
import System.Environment
import Debug.Trace
import Data.List
import Control.DeepSeq

import Data.Map (Map)
import qualified Data.Map as Map

-- ----------------------------------------------------------------------------

-- <<Talk
newtype Talk = Talk Int
  deriving (Eq,Ord)

instance NFData Talk where
  rnf (Talk x) = x `seq` ()

instance Show Talk where
  show (Talk t) = show t
-- >>

-- <<Person
data Person = Person
  { name  :: String
  , talks :: [Talk]
  }
  deriving (Show)
-- >>

-- <<TimeTable
type TimeTable = [[Talk]]
-- >>

-- ----------------------------------------------------------------------------
-- The parallel skeleton

-- <<search_type
search :: ( partial -> Maybe solution )   -- <1>
       -> ( partial -> [ partial ] )      -- <2>
       -> partial                         -- <3>
       -> [solution]                      -- <4>
-- >>

-- <<search
search finished refine emptysoln = generate emptysoln
  where
    generate partial
       | Just soln <- finished partial = [soln]
       | otherwise  = concat (map generate (refine partial))
-- >>

-- ----------------------------------------------------------------------------

-- <<Partial
type Partial = (Int, Int, [[Talk]], [Talk], [Talk], [Talk])
-- >>

-- <<timetable
timetable :: [Person] -> [Talk] -> Int -> Int -> [TimeTable]
timetable people allTalks maxTrack maxSlot =
  search finished refine emptysoln
 where
  emptysoln = (0, 0, [], [], allTalks, allTalks)

  finished (slotNo, trackNo, slots, slot, slotTalks, talks)
     | slotNo == maxSlot = Just slots
     | otherwise         = Nothing

  clashes :: Map Talk [Talk]
  clashes = Map.fromListWith union
     [ (t, ts)
     | s <- people
     , (t, ts) <- selects (talks s) ]

  refine (slotNo, trackNo, slots, slot, slotTalks, talks)
     | trackNo == maxTrack = [(slotNo+1, 0, slot:slots, [], talks, talks)]
     | otherwise =
         [ (slotNo, trackNo+1, slots, t:slot, slotTalks', talks')
         | (t, ts) <- selects slotTalks
         , let clashesWithT = Map.findWithDefault [] t clashes
         , let slotTalks' = filter (`notElem` clashesWithT) ts
         , let talks' = filter (/= t) talks
         ]
-- >>

-- ----------------------------------------------------------------------------
-- Utils

-- <<selects
selects :: [a] -> [(a,[a])]
selects xs0 = go [] xs0
  where
   go xs [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys
-- >>

-- ----------------------------------------------------------------------------
-- Benchmarking / Testing

bench :: Int -> Int -> Int -> Int -> Int -> StdGen
      -> ([Person],[Talk],[TimeTable])

bench nslots ntracks ntalks npersons c_per_s gen =
  (persons,talks, timetable persons talks ntracks nslots)
 where
  total_talks = nslots * ntracks

  talks = map Talk [1..total_talks]
  persons = mkpersons npersons gen

  mkpersons :: Int -> StdGen -> [Person]
  mkpersons 0 g = []
  mkpersons n g = Person ('P':show n) (take c_per_s cs) : rest
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

test = timetable testPersons cs 2 2
 where
   cs@[c1,c2,c3,c4] = map Talk [1..4]

   testPersons =
    [ Person "P" [c1,c2]
    , Person "Q" [c2,c3]
    , Person "R" [c3,c4]
    ]

test2 n m = timetable testPersons cs m n
 where
   cs = map Talk [1 .. (n * m)]

   testPersons =
    [ Person "1"  (take n cs)
    ]
