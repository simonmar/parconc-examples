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
  new_hole <- newEmptyMVar
  mask_ $ do
    old_hole <- takeMVar writeVar
    putMVar old_hole (Item val new_hole)
    putMVar writeVar new_hole
-- >>

-- <<readChan
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \read_end -> do
    (Item val new_read_end) <- readMVar read_end
    return (new_read_end, val)
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
