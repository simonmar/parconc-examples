module WindowManager where

import Control.Concurrent.STM
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

data Window
instance Eq Window
instance Ord Window

data Desktop
instance Eq Desktop
instance Ord Desktop

type Display = Map Desktop (TVar (Set Window))

-- <<moveWindowSTM
moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)
 where
  ma = disp ! a
  mb = disp ! b
-- >>

-- <<moveWindow
moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b
-- >>

-- <<swapWindows
swapWindows :: Display
            -> Window -> Desktop
            -> Window -> Desktop
            -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a
-- >>

render :: Set Window -> IO ()
render = undefined

-- <<UserFocus
type UserFocus = TVar Desktop
-- >>

-- <<getWindows
getWindows :: Display -> UserFocus -> STM (Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar (disp ! desktop)
-- >>

-- <<renderThread
renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus    -- <1>
  loop wins                                     -- <2>
 where
  loop wins = do                                -- <3>
    render wins                                 -- <4>
    next <- atomically $ do
               wins' <- getWindows disp focus   -- <5>
               if (wins == wins')               -- <6>
                   then retry                   -- <7>
                   else return wins'            -- <8>
    loop next
-- >>

