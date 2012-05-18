import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char

main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  tvar <- newTVarIO ""
  forkIO (render tvar)
  forkIO (correcter tvar)
  forever $ do
    c <- getChar
    atomically $ do
      s <- readTVar tvar
      writeTVar tvar (s ++ [c])

correcter :: TVar String -> IO ()
correcter tvar = loop ""
 where
  loop current = do
    new <- atomically $ do
       s <- readTVar tvar
       when (s == current) retry
       let s' = correct s
       when (s' == s) retry
       writeTVar tvar s'
       return s'
    loop new

render :: TVar String -> IO ()
render tvar = loop ""
 where
  printit current s = do
     putStr (replicate (length current) '\8')
     putStr s

  loop current = do
    new <- atomically $ do
       s <- readTVar tvar
       when (s == current) retry
       return s
    printit current new
    loop new

-- This function applies an arbitrary function to the input string.
-- It could do all kinds of funky stuff, but right now all it does is
-- capitalise the first character after a full stop.
--
correct :: String -> String
correct s = go s
 where
  go "" = ""
  go ('.':' ':c:s) | isLower c = '.':' ': toUpper c: go s
  go (c:cs) = c : go cs

