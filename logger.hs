import Control.Concurrent
import Control.Monad

-- -----------------------------------------------------------------------------

-- <<Logger
data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())
-- >>

-- <<initLogger
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l
-- >>

-- <<logger
logger :: Logger -> IO ()
logger (Logger m) = forever $ do
  cmd <- takeMVar m
  case cmd of
    Message msg ->
      putStrLn msg
    Stop s -> do 
      putStrLn "logger: stop"
      putMVar s ()
-- >>

-- <<logMessage
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)
-- >>

-- <<logStop
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
-- >>

-- <<main
main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
-- >>
