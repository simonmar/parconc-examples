import Control.Concurrent

-- <<modifyTwo
modifyTwo :: MVar a -> MVar b -> (a -> b -> IO (a,b)) -> IO ()
modifyTwo ma mb f =
  modifyMVar_ mb $ \b ->
    modifyMVar ma $ \a -> f a b
-- >>

main = do
  ma <- newMVar 'a'
  mb <- newMVar 'b'
  modifyTwo ma mb (\a b -> return (succ a, succ b))
  readMVar ma >>= print
  readMVar mb >>= print
