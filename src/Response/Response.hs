{-# LANGUAGE OverloadedStrings #-}

module Response.Response(
      Response(..)
    , getStatusCode
    , getReasonPhrase
    , getVersion
    , getHeaders
    , getContent
    , buildOkResponse
    , buildNotFoundResponse
    , buildInternalServerErrorResponse
    , toByteString
) where
import qualified Response.StatusCode as SC
import Data.ByteString.Char8 as BS

type Header = (String, [String])
type Headers = [Header]

http11 = "HTTP/1.1"

buildOkResponse :: ByteString -> Headers -> Response
buildOkResponse fileContents headers = Response SC.ok http11 headers fileContents

buildNotFoundResponse :: Headers -> Response
buildNotFoundResponse headers = Response SC.notFound http11 headers "<html><head><title>Not found</title></head><body>The requested resource is not available</body></html>"

buildInternalServerErrorResponse :: Headers -> Response
buildInternalServerErrorResponse headers = Response SC.notFound http11 headers ""

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
    buildContentLength response =  append "Content-Length: " $ BS.pack . show . BS.length $ getContent response
