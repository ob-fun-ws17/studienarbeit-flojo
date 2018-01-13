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
) where
import Response.StatusCode as SC
import Data.ByteString

type Header = (String, [String])
type Headers = [Header]

http11 = "HTTP/1.1"

buildOkResponse :: ByteString -> Headers -> Response
buildOkResponse fileContents headers = Response ok http11 headers fileContents

buildNotFoundResposne :: Headers -> Response
buildNotFoundResponse headers = Response notFound http11 headers ""

buildInternalServerErrorResponse :: Headers -> Response
buildInternalServerErrorResponse headers = Response notFound http11 headers ""

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
toByteString response = buildResponseLine request

buildResponseLine request = append (append (pack (version request)) (append (pack (getStatusCode response) (pack (getReasonPhrase response))) "\r\n"

buildHeaders (++)
buildHeader (k, [b]) = append (append (append k ": ") v) "\r\n"
buildHeader h = buildHeader
getContentLength response = ["Content-Length", [length $ getContent response])
