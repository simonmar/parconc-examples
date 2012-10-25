-- <<reminders
import Control.Concurrent
import Text.Printf
import Control.Monad

main =
  forever $ do
    l <- getLine                  -- <1>
    forkIO $ setReminder (read l) -- <2>

setReminder :: Int -> IO ()
setReminder t = do
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)                   -- <3>
  printf "%d seconds is up! BING!\BEL\n" t -- <4>
-- >>

