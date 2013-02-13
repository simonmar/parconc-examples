module ParList where

import Control.Parallel.Strategies hiding (parList)

-- <<parList
parList :: Strategy a -> Strategy [a]
parList strat []     = return []
parList strat (x:xs) = do
  x'  <- rparWith strat x               -- <1>
  xs' <- parList strat xs
  return (x':xs')
-- >>
