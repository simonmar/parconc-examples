{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
import Remote
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import Data.DeriveTH
import Data.Binary
import Data.Typeable

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving Typeable

$( derive makeBinary ''Message )
-- >>

-- <<pingServer
pingServer :: ProcessM ()
pingServer = do
  Ping from <- expect
  mypid <- getSelfPid
  send from (Pong mypid)
-- >>

-- <<remotable
$( remotable ['pingServer] )
-- >>

-- <<master
master :: ProcessM ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node pingServer__closure

  mypid <- getSelfPid
  say $ printf "pinging %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong."
  terminate
-- >>

-- <<main
main = remoteInit (Just "config") [Main.__remoteCallMetaData] $
         \_ -> master
-- >>
