{-# LANGUAGE OverloadedStrings #-}
import Remote
import DatabaseRepl  (Database, createDB, get, set, rcdata)
import Worker
import Control.Monad.IO.Class
import Control.Monad
import System.IO

main = remoteInit (Just "config") rcdata initialProcess

initialProcess :: String -> ProcessM ()
initialProcess "WORKER" = receiveWait []
initialProcess "MASTER" = master

master :: ProcessM ()
master = do
  db <- createDB

  f <- liftIO $ readFile "Database.hs"
  let ws = words f

  zipWithM_ (set db) ws (tail ws)

  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print

  forever $ do
    l <- liftIO $ do putStr "key: "; hFlush stdout; getLine
    when (not (null l)) $ do
      r <- get db l
      liftIO $ putStrLn ("response: " ++ show r)

  return ()
