{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Directory
import Control.Concurrent
import System.FilePath
import System.Environment
import Data.List hiding (find)
import GHC.Conc (getNumCapabilities)
import Text.Printf

import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Monad.IO.Class

import Control.Exception

-- <<main
main = do
  [s,d] <- getArgs
  runParIO (find s d) >>= print
-- >>

-- <<find
find :: String -> FilePath -> ParIO (Maybe FilePath)
find s d = do
  fs <- liftIO $ getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'
       foldr (subfind s) dowait ps []
 where
   dowait vs = loop (reverse vs)

   loop [] = return Nothing
   loop (v:vs) = do
      r <- get v
      case r of
        Nothing -> loop vs
        Just a  -> return (Just a)
-- >>

-- <<subfind
subfind :: String -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        ->  [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)

subfind s p inner ivars = do
  isdir <- liftIO $ doesDirectoryExist p
  if not isdir
     then inner ivars
     else do v <- new                   -- <1>
             fork (find s p >>= put v)  -- <2>
             inner (v : ivars)          -- <3>
-- >>
