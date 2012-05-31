import Control.Concurrent
import System.Random
import Text.Printf
import System.IO
import System.Timeout

main = do
 hSetBuffering stdout NoBuffering
 loop 0
 where
   loop :: Int -> IO ()
   loop score = do
     printf "current score: %d\n" score
     a <- randomRIO (0,50 :: Int)
     b <- randomRIO (0,50)
     n <- randomRIO (0,2)
     let op  = [(+),(-),(*)] !! n
     let sop = ["+","-","*"] !! n
     printf "%d %s %d = " a sop b
     l <- getLine
     if read l == a `op` b
        then printf "CORRECT.\n" >> loop (score + 1)
        else printf "WRONG.\n" >> loop (max 0 (score-1))
