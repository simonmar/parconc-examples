import Control.Concurrent
import Control.Monad

-- -----------------------------------------------------------------------------

-- <<logger
data LogCommand = Message String | Stop (MVar ())

data Logger = Logger (MVar LogCommand)

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = forever $ do
  cmd <- takeMVar m
  case cmd of
    Message msg -> do 
      threadDelay 10000
      putStrLn msg
    Stop s -> do 
      putStrLn "logger: stop"
      putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
-- >>
