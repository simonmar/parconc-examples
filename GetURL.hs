-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Simple wrapper around HTTP, allowing proxy use

module GetURL (getURL) where

import Network.HTTP.Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Control.Applicative -- for GHC < 7.10

getURL :: String -> IO ByteString
getURL url = L.toStrict <$> simpleHttp url
