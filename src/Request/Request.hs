{-# LANGUAGE OverloadedStrings #-}
module Request.Request (
    parseRequest
  , parseToString
  , RL.RequestLine()
  , Request(..)
  , path
) where

import Data.ByteString as BS
import System.IO as IO
import Data.List.Split as Split
import Data.Map as Map
import qualified Request.RequestLine as RL
import Data.ByteString.Char8 as BS2

data Request = Request {requestLine :: RL.RequestLine, h::[(BS.ByteString, [BS.ByteString])]} deriving (Show)

method :: Request -> BS.ByteString
path :: Request -> BS.ByteString
version :: Request -> BS.ByteString
headers :: Request -> [(BS.ByteString, [BS.ByteString])]

method (Request line _) = RL.method line
path (Request line _) = RL.path line
version (Request line _) = RL.version line
headers (Request _ h) = h

endOfRequest = "\r"
headerSeparator = ':'

parseRequest :: Handle -> IO Request
parseRequest handle = do
        let list = []
        requestString <- parseToString handle list
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
      --headers = parseHeaders $ Prelude.tail requestLines

parseToString :: Handle -> [BS.ByteString] -> IO [BS.ByteString]
parseToString handle allLines =
  do line <- BS.hGetLine handle
     if line == BS2.pack endOfRequest
        then return allLines
        else do
          Prelude.putStrLn $ BS2.unpack line
          let allLines = allLines ++ [line]
          allLines <- parseToString handle allLines
          return allLines

parseHeaders :: [BS.ByteString] -> [(BS.ByteString, [BS.ByteString])]
parseHeaders headers =
  [parseHeader line | line <- headers]

parseHeader :: BS.ByteString -> (BS.ByteString, [BS.ByteString])
parseHeader line =
  (key, v)
  where
    values = BS2.split headerSeparator line
    key = Prelude.head values
    v = Prelude.tail values
