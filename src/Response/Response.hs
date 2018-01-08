module Response.Response(
      Response(..)
    , getStatusCode
    , getReasonPhrase
    , getVersion
    , getHeaders
    , getContent
) where
import qualified Response.StatusCode as SC
import Data.ByteString

type Header = (String, [String])
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
