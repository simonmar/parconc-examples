module BingTranslate (
    detectLanguage,
    getLanguages,
    translateText
  ) where

import GetURL
import Text.XML.Light
import Text.Printf
import Data.ByteString.UTF8 as UTF8

-- Simon Marlow's AppId, please get your own from http://msdn.microsoft.com/en-us/library/ff512386.aspx
myAppId = "3157F269F56493B5581BAA4AE7B99C35052DA38B"

-- New v2 API:
getlanguagesUri = "http://api.microsofttranslator.com/v2/Http.svc/GetLanguagesForTranslate?appId=" ++ myAppId
detectUri = "http://api.microsofttranslator.com/v2/Http.svc/Detect?appId=" ++ myAppId
translateUri = "http://api.microsofttranslator.com/v2/Http.svc/Translate?appId=" ++ myAppId

getLanguages :: IO [String]
getLanguages = do
  r <- getURL getlanguagesUri
  return (getLangs (parseXML r))

detectLanguage :: String -> IO String
detectLanguage text = do
  r <- getURL (detectUri ++ "&text=" ++ text)
  return (head (map getString (parseXML r)))

translateText :: String -> String -> String -> IO String
translateText text fromLang toLang = do
  r <- getURL (printf "%s&text=%s&from=%s&to=%s" translateUri text fromLang toLang)
  return (concat (map getString (parseXML (UTF8.toString r))))

-----------------------------------------------------------------------------
-- Hacky XML decoding

-- This will fail utterly if the input is not in the right form.

getLangs :: [Content] -> [String]
getLangs [Elem (Element {elName = QName { qName = "ArrayOfstring" }, elContent = strs })]
  = map getString strs

getString :: Content -> String
getString (Elem (Element {elName = QName { qName = "string" }, elContent = [Text (CData { cdData = str })]})) = str


-- OLD stuff that we might need later:

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
