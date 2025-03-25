module NetworkUtils (
    withSocketsDo, listenOn, NetworkUtils.accept
  ) where

import Control.Exception
import Network.Run.TCP
import Network.Socket as Socket
import System.IO

listenOn :: Int -> (Socket -> IO a) -> IO a
listenOn port server = do
  addr <- resolve Stream Nothing (show port) [AI_PASSIVE]
  bracket (openTCPServerSocket addr) close server

accept :: Socket -> ((Handle, SockAddr) -> IO a) -> IO a
accept sock server = do
  bracketOnError (Socket.accept sock) (close . fst) $ \(conn, peer) -> do
    handle <- socketToHandle conn ReadWriteMode
    server (handle, peer)
