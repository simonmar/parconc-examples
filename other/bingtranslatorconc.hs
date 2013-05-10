{-# LANGUAGE PatternGuards #-}
import Network.HTTP hiding (postRequest)
import Control.Exception
import Control.Monad
import Text.Printf
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Network.URI
import System.Environment
import Control.Concurrent
import Prelude hiding (catch)
import Network.HTTP
import Network.Browser
import Network.URI
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.XML.Light

import BingTranslate as Bing

main = do
  [text] <- fmap (fmap (B.unpack . UTF8.fromString)) getArgs

  languages <- Bing.getLanguages

  fromLang <- Bing.detectLanguage text
  printf "\"%s\" appears to be in language \"%s\"\n" text fromLang

  translations <- concurrently $
    map (\lang -> do r <- Bing.translateText text fromLang lang; return (lang,r))
      (filter (/= fromLang) languages)

  forM_ translations $ \(lang,str) ->
    printf "%s: %s\n" lang str

-----------------------------------------------------------------------------
-- Our Async API:

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyMVar
   t <- forkIO ((do r <- action; putMVar var (Right r))
                  `catch` \e -> putMVar var (Left e))
   return (Async t var)

wait :: Async a -> IO (Either SomeException a)
wait (Async t var) = readMVar var

waitFail :: Async a -> IO a
waitFail (Async t var) = do
  e <- readMVar var
  case e of
     Left err -> throw err
     Right a  -> return a

cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled

concurrently :: [IO a] -> IO [a]
concurrently ios = mapM async ios >>= mapM waitFail

-- httpRequestUTF8 :: String -> Maybe ByteString -> IO String
-- httpRequestUTF8 url body = do
--   let request_hdr = postRequest url
--       request | Just text <- body =  request_hdr `addRequestContent` text
--               | otherwise         =  request_hdr
--   s <- simpleHTTP request >>= getResponseBody
--   return (chopBOM (UTF8.toString s))
-- 
-- chopBOM ('\xfeff' : s) = s
-- chopBOM s = s
-- 
-- postRequest :: HStream a => String -> Request a
-- postRequest urlString = 
--   case parseURI urlString of
--     Nothing -> error ("postRequest: Not a valid URL - " ++ urlString)
--     Just u  -> mkRequest POST u
-- 
-- addRequestContent :: Request ByteString -> ByteString -> Request ByteString
-- addRequestContent rq content
--   = rq {rqBody = content,
--         rqHeaders = 
--           mkHeader HdrContentType "text/plain" :
--           mkHeader HdrContentLength (show (B.length content)) : rqHeaders rq
--        }
