import System.Directory
import Control.Concurrent
import System.FilePath
import Control.Concurrent.Async
import System.Environment
import Data.List hiding (find)

main = do
  [s,d] <- getArgs
  find s d >>= print

-- <<find
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'         -- <1>
       foldr (subfind s) dowait ps []   -- <2>
 where
   dowait as = loop (reverse as)        -- <3>

   loop [] = return Nothing
   loop (a:as) = do                     -- <4>
      r <- wait a
      case r of
        Nothing -> loop as
        Just a  -> return (Just a)
-- >>

-- <<subfind
subfind :: String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)

subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
     then inner asyncs
     else withAsync (find s p) $ \a -> inner (a:asyncs)
-- >>
