import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, dupChan)
import Control.Exception

-- <<Stream
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)
-- >>

-- <<Chan
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))
-- >>

-- <<newChan
newChan :: IO (Chan a)
newChan = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)
-- >>

-- <<writeChan
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  mask_ $ do
    oldHole <- takeMVar writeVar
    putMVar oldHole (Item val newHole)
    putMVar writeVar newHole
-- >>

-- <<readChan
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \readEnd -> do
    (Item val newReadEnd) <- readMVar readEnd
    return (newReadEnd, val)
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
