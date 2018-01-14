-- | This module describes HTTP status codes.
module Response.StatusCode (
    StatusCode(..)
  , Code
  , getStatusCode
  , getReasonPhrase
  , ok
  , notFound
) where

data Code = Continue | NotFound | Ok

-- | Represents an HTTP StatusCode
data StatusCode = StatusCode { code :: Code, statusCode :: Integer, reasonPharse :: [Char]}

-- | Extract the numeric status code from a StatusCode
getStatusCode :: StatusCode -> Integer
getStatusCode (StatusCode _ value _) = value

-- | Extract the reason pharse from a StatusCode
getReasonPhrase :: StatusCode -> [Char]
getReasonPhrase (StatusCode _ _ phrase) = phrase


ok = StatusCode Ok 200 "OK"
notFound = StatusCode NotFound 404 "Not Found"
