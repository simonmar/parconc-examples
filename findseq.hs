{-# OPTIONS_GHC -Wall #-}
import System.Directory
import System.FilePath
import System.Environment
import Data.List hiding (find)

-- <<main
main :: IO ()
main = do
  [s,d] <- getArgs
  r <- find s d
  print r
-- >>

-- <<find
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d                         -- <1>
  let fs' = sort $ filter (`notElem` [".",".."]) fs    -- <2>
  if any (== s) fs'                                    -- <3>
     then return (Just (d </> s))
     else loop fs'                                     -- <4>
 where
  loop [] = return Nothing                             -- <5>
  loop (f:fs)  = do
    let d' = d </> f                                   -- <6>
    isdir <- doesDirectoryExist d'                     -- <7>
    if isdir
       then do r <- find s d'                          -- <8>
               case r of
                 Just _  -> return r                   -- <9>
                 Nothing -> loop fs                    -- <10>
       else loop fs                                    -- <11>
-- >>
