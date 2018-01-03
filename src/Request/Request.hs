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

data Request = Request {requestLine :: RL.RequestLine, h::[(String,String)]} deriving (Show)


method :: Request -> String
path :: Request -> String
version :: Request -> String
headers :: Request -> [(String,String)]

method (Request line _) = RL.method line
path (Request line _) = RL.path line
version (Request line _) = RL.version line
headers (Request _ h) = h

parseRequest :: String -> Request
parseRequest requestString =
  Request requestLine headers
  where requestLine =
          case RL.fromString $ getRequestLineString requestString of
            Right x -> x
            Left err -> error "Failed"
        headers = getHeaderTupelList $ getRequestHeaderString requestString
        l = Split.splitOn "\r\n" requestString

parseToString :: Handle -> IO String
parseToString handle =
  do r1 <- BSL.hGet handle 1
     rRest <- BSL.hGetNonBlocking handle 1024
     let result = show $ BSL.append r1 rRest
     return result

getRequestLineString :: String -> String
getRequestLineString request = Prelude.head $ Split.splitOn "\\r\\n" request

getRequestHeaderString :: String -> [String]
getRequestHeaderString request = Prelude.tail $ Split.splitOn "\\r\\n" request

parseHeaders :: String -> [String]
parseHeaders headers = Split.splitOn "\n" headers

getHeaderTupelList :: [String] -> [(String, String)]
getHeaderTupelList headers = [toTupel $ Split.splitOn ":" h | h <- headers, Prelude.length h > 1]

toTupel :: [String] -> (String, String)
toTupel [a,b] = (a,b)
toTupel [a,b,c] = (a,comb)
  where comb = b ++ ":" ++ c
