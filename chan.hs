import Control.Concurrent hiding (Chan, newChan, readChan, writeChan)

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
  old_hole <- takeMVar writeVar
  putMVar writeVar new_hole
  putMVar old_hole (Item val new_hole)
-- >>

-- <<readChan
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar
  Item val new <- takeMVar stream
  putMVar readVar new
  return val
-- >>

-- <<dupChan
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole       <- takeMVar writeVar
  putMVar writeVar hole
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
