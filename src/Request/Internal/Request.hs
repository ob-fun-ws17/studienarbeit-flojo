{-# LANGUAGE OverloadedStrings #-}
-- | bundles internal functions and datatypes to parse a Http-Request.
module Request.Internal.Request (
    parseRequest
  , parseRequestFromString
  , parseToString
  , parseHeaders
  , parseHeader
  , fromString
  , Request(..)
  , RequestLine(..)
)
where

import Request.Error
import Control.Monad
import Data.ByteString as BS
import System.IO as IO
import Data.List.Split as Split
import Data.Map as Map
import Data.ByteString.Char8 as BS2

-- | Monad for error handling while parsing a request.
type ParseMonad = Either ParseError

-- | The constructor of the request. A request contains a requestline (methond, path and http-version) and a list of headers.
data Request = Request {requestLine::RequestLine, headers::[(BS.ByteString, [BS.ByteString])]} deriving (Show, Eq)

-- ¦ The construcor for a request line. It contains the fields method, path and version.
data RequestLine = RequestLine {method :: ByteString, path :: ByteString, version :: ByteString} deriving (Show, Eq)

headerSeparator = ':'

-- | top level function, that is used to parse a request from a handle.
parseRequest :: Handle -> IO Request
parseRequest handle = do
        let list = []
        requestString <- parseToString handle list
        let request = parseRequestFromString requestString
        return request


-- | Top level function for parsing a byteString to a RequestLine.
fromString :: ByteString -> ParseMonad RequestLine
fromString line = toRequestLine fields
  where fields = BS2.split ' ' line

-- | Pattern matching for finding malformed requests.
toRequestLine :: [ByteString] -> ParseMonad RequestLine
toRequestLine ["GET", p, "HTTP/1.1"] = Right $ RequestLine "GET" p "HTTP/1.1"
toRequestLine [m, _, "HTTP/1.1"] = Left $ HttpMethodNotSupported m
toRequestLine ["GET", p, v] = Left $ HttpVersionNotSupported (BS.append (BS.append (BS.append "GET " p) " ") v)
toRequestLine [m, p, v] = Left $ RequestLineMalformed (BS.append (BS.append m p) v)
toRequestLine _ = Left $ UnknownParseError ""

-- | Parses a request from a bytestring.
parseRequestFromString :: [BS.ByteString] -> Request
parseRequestFromString requestLines =
  Request requestLine headers
  where
      requestLine =
          case fromString $ Prelude.head requestLines of
            Right x -> x
            Left err -> error $ show err
      headers = parseHeaders $ Prelude.tail requestLines

-- | parses the request from a handle line by line to a list of bytesstrings.
parseToString :: Handle -> [BS.ByteString] -> IO [BS.ByteString]
parseToString handle allLines =
  do line <- IO.hGetContents handle
     let lineList = splitLines line
     return lineList

-- | splits a request-string into a list, each entry being a line of the request.
splitLines :: String -> [ByteString]
splitLines string = Prelude.map BS2.pack (splitOn "\r\n" string)

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
