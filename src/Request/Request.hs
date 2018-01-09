module Request.Request (
    parseRequest
  , parseToString
  , RL.RequestLine()
  , Request(..)
  , path
) where

import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import System.IO as IO
import Data.List.Split as Split
import Data.Map as Map
import qualified Request.RequestLine as RL
import Data.ByteString.Lazy.Char8 as BS2

data Request = Request {requestLine :: RL.RequestLine, h::[Header]} deriving (Show)
type Header = (String, [String])

method :: Request -> BS.ByteString
path :: Request -> BS.ByteString
version :: Request -> BS.ByteString
headers :: Request -> [Header]

method (Request line _) = RL.method line
path (Request line _) = RL.path line
version (Request line _) = RL.version line
headers (Request _ h) = h

newLine = "\\r\\n"
headerSeparator = ":"
headerValueSeparator = ";"


parseRequest :: Handle -> IO Request
parseRequest handle = do
        requestString <- parseToString handle
        let request = parseRequestFromString requestString
        return request


parseRequestFromString :: [BS.ByteString] -> Request
parseRequestFromString requestLines =
  Request requestLine headers
  where
      requestLine =
          case RL.fromString $ Prelude.head requestLines of
            Right x -> x
            Left err -> error $ show err
      headers = []

parseToString :: Handle -> IO [BS.ByteString]
parseToString handle =
  do line <- BS.hGetLine handle
     return [line]

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
