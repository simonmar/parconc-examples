module TMVar where

import Control.Concurrent.STM hiding (TMVar, takeTMVar)

-- <<TMVar
newtype TMVar a = TMVar (TVar (Maybe a))
-- >>

newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  t <- newTVar (Just a)
  return (TMVar t)

-- <<newEmptyTMVar
newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)
-- >>

-- <<takeTMVar
takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t                       -- <1>
  case m of
    Nothing -> retry                    -- <2>
    Just a  -> do
      writeTVar t Nothing               -- <3>
      return a
-- >>

-- <<putTMVar
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do
      writeTVar t (Just a)
      return ()
    Just _  -> retry
-- >>

-- <<takeEitherTMVar
takeEitherTMVar :: TMVar a -> TMVar b -> STM (Either a b)
takeEitherTMVar ma mb =
  fmap Left (takeTMVar ma)
    `orElse`
  fmap Right (takeTMVar mb)
-- >>
