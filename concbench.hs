import System.Environment
import ConcurrentUtils
import Control.Monad

main = do
  [n] <- fmap (fmap read) getArgs
  replicateM_ n $ conc (return 1) (return 2)

-- test the performance of race_
conc = race_
--conc a b = waitEitherThrowCancel <$> async a <*> async b

