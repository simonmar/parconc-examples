{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Data.Unique
import Control.Exception
import Data.Typeable
import Control.Concurrent.Async

-- <<timeout
timeout :: Int -> IO a -> IO (Maybe a)
timeout n m
    | n <  0    = fmap Just m
    | n == 0    = return Nothing
    | otherwise = do
        r <- race (threadDelay n) m
        case r of
          Left _  -> return Nothing
          Right a -> return (Just a)
-- >>

main = (timeout 200000 $ timeout 100000 $ timeout 300000 $ threadDelay 1000000) >>= print
