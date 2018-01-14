{-# LANGUAGE OverloadedStrings #-}
-- | Module modeling an HTTP Response and its transformation to a BysteString.
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

data Response = Response { statusCode :: SC.StatusCode, version :: ByteString, headers :: Headers, content :: ByteString }

-- | Extract the response status code.
getStatusCode :: Response -> Integer
getStatusCode (Response status _ _ _) = SC.getStatusCode status

-- | Extract the response reaso phrase.
getReasonPhrase :: Response -> String
getReasonPhrase (Response status _ _ _) = SC.getReasonPhrase status

-- | Extract the http version.
getVersion :: Response -> ByteString
getVersion (Response _ version _ _) = version

-- | Extract the response content
getContent :: Response -> ByteString
getContent (Response _ _ _ content) = content

-- | Extract the response headers.
getHeaders :: Response -> Headers
getHeaders (Response _ _ headers _) = headers

-- | Transform a response to its ByteString representation.
toByteString :: Response -> ByteString
toByteString response = Prelude.foldr append " " [buildResponseLine response, buildContentLength response, "\r\n\r\n", getContent response]
  where
    buildResponseLine response = Prelude.foldr append " " [getVersion response, " ",  (pack . show . getStatusCode) response, " ", (pack . show . getReasonPhrase) response, "\r\n"]
    buildContentLength response =  append "Content-Length: " $ BS.pack . show . BS.length $ getContent response

-- | Template for building an OK response where only the request content is missing.
buildOkResponse :: Headers -> (ByteString -> Response)
buildOkResponse headers = Response SC.ok http11 headers

-- | Template for building a NotFound response where only the request content is missing.
buildNotFoundResponse :: Headers -> (ByteString -> Response)
buildNotFoundResponse headers = Response SC.notFound http11 headers

-- | Template for building a InternalServerError Response where only the request content is missing.
buildInternalServerErrorResponse :: Headers -> (ByteString -> Response)
buildInternalServerErrorResponse headers = Response SC.notFound http11 headers
