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
  -- continue = StatusCode Continue 100 "Continue"
  -- | Information status codes (1xx)
  --data Informational = Continue

  -- | Success status codes (2xx)
  --data Success = Ok

  -- | Redication status codes (3xx)
  --data Redication = MovedPermanently

  -- | Client error status codes (4xx)
  --data ClientErrors = NotFound

  -- | Server error status codes (5xx)
  --data ServerError = InternalServerError | NotImplements | BadGateway | ServiceUnavailable

  -- | Http response status codes
  --data Code = Informational | Success | Redication | ClientErrors | ServerErrors

  -- | Retrieve a status codes numeric representation.
  --code :: Code -> Integer

  --code Ok = 200
  --code MovedPermanently = 302
  --code NotFound = 404
  --code InternalServerError = 500

  --code Continue = 101

  -- | Retrieve a status codes string representation.
  --reason :: Code -> [Char]
  --reason Continue = "Continoue"
  --reason Ok = "OK"
  --reason MovedPermanently = "Moved Permanently"
  --reason NotFound = "Not Found"
  --reason InternalServerError = "Internal Server Error"
