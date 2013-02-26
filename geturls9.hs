import GetURL

import Async

import qualified Data.ByteString as B

-----------------------------------------------------------------------------

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

-- <<main
main = do
  xs <- foldr conc (return []) (map getURL sites)
  print (map B.length xs)
 where
  conc ioa ioas = do
    (a,as) <- concurrently ioa ioas
    return (a:as)
-- >>
