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
data Message = Ping (SendPort ProcessId)
  deriving Typeable

$( derive makeBinary ''Message )
-- >>

-- <<pingServer
pingServer :: ProcessM ()
pingServer = do
  Ping chan <- expect
  mypid <- getSelfPid
  sendChannel chan mypid
-- >>

-- <<remotable
$( remotable ['pingServer] )
-- >>

-- <<master
master :: ProcessM ()
master = do
  peers <- getPeers

  let workers = findPeerByRole peers "WORKER"

  ps <- forM workers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid pingServer__closure

  mypid <- getSelfPid

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChannel
    send pid (Ping sendport)
    return recvport

  forM_ ports $ \port -> do
     p <- receiveChannel port
     return ()

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess

initialProcess :: String -> ProcessM ()
initialProcess "WORKER" = receiveWait []
initialProcess "MASTER" = master
-- >>
