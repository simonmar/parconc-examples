import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, dupChan)

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
  Item val new <- readMVar stream  -- <1>
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

-- <<unGetChan
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val = do
  new_read_end <- newEmptyMVar             -- <1>
  read_end <- takeMVar readVar             -- <2>
  putMVar new_read_end (Item val read_end) -- <3>
  putMVar readVar new_read_end             -- <4>
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
  c2 <- dupChan c
  writeChan c 'b'
  readChan c >>= print
  readChan c2 >>= print
