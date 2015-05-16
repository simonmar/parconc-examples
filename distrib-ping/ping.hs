{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import DistribUtils

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)          -- <1>

instance Binary Message                 -- <2>
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping from <- expect                              -- <1>
  say $ printf "ping received from %s" (show from) -- <2>
  mypid <- getSelfPid                              -- <3>
  send from (Pong mypid)                           -- <4>
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: Process ()
master = do
  node <- getSelfNode                               -- <1>

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)  -- <2>

  mypid <- getSelfPid                               -- <3>
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)                             -- <4>

  Pong _ <- expect                                  -- <5>
  say "pong."

  terminate                                         -- <6>
-- >>

-- <<main
main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable
-- >>
