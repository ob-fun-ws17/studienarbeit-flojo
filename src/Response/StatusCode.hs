module Response.StatusCode (
    StatusCode(..)
  , Code
  , getStatusCode
  , getReasonPhrase
  , ok
  , notFound
) where

  data Code = Continue | NotFound | Ok
  data StatusCode = StatusCode { code :: Code, statusCode :: Integer, reasonPharse :: [Char]}

  getStatusCode :: StatusCode -> Integer
  getStatusCode (StatusCode _ value _) = value

  getReasonPhrase :: StatusCode -> [Char]
  getReasonPhrase (StatusCode _ _ phrase) = phrase

  ok = StatusCode Ok 200 "OK"
  notFound = StatusCode NotFound 404 "Not Found"
