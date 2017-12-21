module Request (
    parse
  , RequestLine(..)
  , Request(..)
) where
  import Data.ByteString.Lazy as BSL
  import System.IO as IO

  data RequestLine = RequestLine {m :: String, p :: String, v :: String}
  data Request = Request { requestLine :: RequestLine }

  method :: RequestLine -> String
  path :: RequestLine -> String
  version :: RequestLine -> String

  method (RequestLine m _ _) = m
  path (RequestLine _ p _) = p
  version (RequestLine _ _ v) = v

  method' :: Request -> String
  path' :: Request -> String
  version' :: Request -> String

  method' (Request line) = method line
  path' (Request line) = path line
  version' (Request line) = version line

  parse :: Handle -> IO()
  parse handle =
    do
      r1 <- BSL.hGet handle 1
      rRest <- BSL.hGetNonBlocking handle 1024
      IO.putStrLn (show (BSL.append r1 rRest))
