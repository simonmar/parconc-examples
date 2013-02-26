{-# LANGUAGE CPP #-}
module ByteStringCompat () where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.DeepSeq

#if !MIN_VERSION_bytestring(0,10,0)

instance NFData ByteString where
  rnf x = B.length x `seq` ()

#endif
