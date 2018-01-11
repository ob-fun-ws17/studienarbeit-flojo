{-# LANGUAGE OverloadedStrings #-}

module Response.Response(
      Response(..)
    , getStatusCode
    , getReasonPhrase
    , getVersion
    , getHeaders
    , getContent
    , toByteString
) where
import qualified Response.StatusCode as SC
import Data.ByteString.Char8 as BS

type Header = (ByteString, [ByteString])
type Headers = [Header]

data Response = Response { statusCode :: SC.StatusCode, version :: ByteString, headers :: Headers, content :: ByteString }

getStatusCode :: Response -> Integer
getStatusCode (Response status _ _ _) = SC.getStatusCode status

getReasonPhrase :: Response -> String
getReasonPhrase (Response status _ _ _) = SC.getReasonPhrase status

getVersion :: Response -> ByteString
getVersion (Response _ version _ _) = version

getContent :: Response -> ByteString
getContent (Response _ _ _ content) = content

getHeaders :: Response -> Headers
getHeaders (Response _ _ headers _) = headers

toByteString :: Response -> ByteString
toByteString response = Prelude.foldr append " " [buildResponseLine response, buildContentLength response, "\r\n\r\n", getContent response]
  where
    buildResponseLine response = Prelude.foldr append " " [getVersion response, " ",  (pack . show . getStatusCode) response, " ", (pack . show . getReasonPhrase) response, "\r\n"]
    buildContentLength response =  append "Content-Length: " $ pack . show . BS.length $ getContent response
