{-# LANGUAGE OverloadedStrings #-}
-- | bundles internal functions and datatypes to parse a Http-Request.
module Request.Internal.Request (
    parseRequest
  , parseRequestFromString
  , parseToString
  , parseHeaders
  , parseHeader
  , method
  , version
  , path
  , headers
  , Request(..)
)
where

import Data.ByteString as BS
import System.IO as IO
import Data.List.Split as Split
import Data.Map as Map
import qualified Request.RequestLine as RL
import Data.ByteString.Char8 as BS2

-- | The constructor of the request. A request contains a requestline (methond, path and http-version) and a list of headers.
data Request = Request {requestLine :: RL.RequestLine, h::[(BS.ByteString, [BS.ByteString])]} deriving (Show, Eq)

-- | function, to get the method of a request.
method :: Request -> BS.ByteString
method (Request line _) = RL.method line

-- | funtion, to get the path of a request.
path :: Request -> BS.ByteString
path (Request line _) = RL.path line

-- | function, to get the http-version of a request.
version :: Request -> BS.ByteString
version (Request line _) = RL.version line

-- ¦ function, to get the list of headers from a request.
headers :: Request -> [(BS.ByteString, [BS.ByteString])]
headers (Request _ h) = h

endOfRequest = "\r"
headerSeparator = ':'

-- | top level function, that is used to parse a request from a handle.
parseRequest :: Handle -> IO Request
parseRequest handle = do
        let list = []
        requestString <- parseToString handle list
        let request = parseRequestFromString requestString
        return request

-- | Parses a request from a bytestring.
parseRequestFromString :: [BS.ByteString] -> Request
parseRequestFromString requestLines =
  Request requestLine headers
  where
      requestLine =
          case RL.fromString $ Prelude.head requestLines of
            Right x -> x
            Left err -> error $ show err
      headers = []
      --headers = parseHeaders $ Prelude.tail requestLines

-- | parses the request from a handle line by line to a list of bytesstrings.
parseToString :: Handle -> [BS.ByteString] -> IO [BS.ByteString]
parseToString handle allLines =
  do line <- BS.hGetLine handle
     return [line]
  {-do line <- BS.hGetLine handle
     if line == BS2.pack endOfRequest
        then return allLines
        else do
          let allLines = allLines ++ [line]
          allLines <- parseToString handle allLines
          return allLines-}

-- | parses the headers from a list of bytestrings, that contain the lines.
parseHeaders :: [BS.ByteString] -> [(BS.ByteString, [BS.ByteString])]
parseHeaders headers =
  [parseHeader line | line <- headers]

-- | reads one header to a tupel (headername, listOfValues)
parseHeader :: BS.ByteString -> (BS.ByteString, [BS.ByteString])
parseHeader line =
  (key, v)
  where
    values = BS2.split headerSeparator line
    key = Prelude.head values
    v = Prelude.tail values
