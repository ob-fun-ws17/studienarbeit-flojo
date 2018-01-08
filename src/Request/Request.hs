module Request.Request (
    parseRequest
  , parseToString
  , RL.RequestLine()
  , Request(..)
) where

import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import System.IO as IO
import Data.List.Split as Split
import Data.Map as Map
import qualified Request.RequestLine as RL

data Request = Request {requestLine :: RL.RequestLine, h::[Header]} deriving (Show)
type Header = (String, [String])

method :: Request -> String
path :: Request -> String
version :: Request -> String
headers :: Request -> [Header]

method (Request line _) = RL.method line
path (Request line _) = RL.path line
version (Request line _) = RL.version line
headers (Request _ h) = h

newLine = "\\r\\n"
headerSeparator = ":"
headerValueSeparator = ";"

parseRequest :: String -> Request
parseRequest requestString =
  Request requestLine headers
  where
      requestLines = Split.splitOn newLine requestString

      requestLine =
          case RL.fromString $ Prelude.head requestLines of
            Right x -> x
            Left err -> error $ show err
      headers = []

parseToString :: Handle -> IO String
parseToString handle =
  do r1 <- BSL.hGet handle 1
     rRest <- BSL.hGetNonBlocking handle 1024
     let result = show $ BSL.append r1 rRest
     return result

parseHeaders :: String -> [Header]
parseHeaders headers = []
  --[parseHeader line | line <- (Split.splitOn newLine headers), h /= newLine]

parseHeader :: String -> Header
parseHeader line =
  (key, v)
  where
    values = Split.splitOn ":" line
    key = Prelude.head values
    v = Prelude.tail values
