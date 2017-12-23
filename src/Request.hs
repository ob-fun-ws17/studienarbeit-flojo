module Request (
    parseRequest
  , parseToString
  , RequestLine(..)
  , Request(..)
) where
  import Data.ByteString.Lazy as BSL
  import Data.ByteString as BS
  import System.IO as IO
  import Data.List.Split as Split
  import Data.Map as Map

  data RequestLine = RequestLine {m :: String, p :: String, v :: String} deriving (Show)
  data Request = Request {requestLine :: RequestLine, h::[(String,String)]} deriving (Show)

  method :: RequestLine -> String
  path :: RequestLine -> String
  version :: RequestLine -> String

  method (RequestLine m _ _) = m
  path (RequestLine _ p _) = p
  version (RequestLine _ _ v) = v

  method' :: Request -> String
  path' :: Request -> String
  version' :: Request -> String
  headers' :: Request -> [(String,String)]

  method' (Request line _) = method line
  path' (Request line _) = path line
  version' (Request line _) = version line
  headers' (Request _ h) = h

  parseRequest :: String -> Request
  parseRequest requestString =
    Request line headers
    where lineString = getRequestLineString requestString
          line = parseRequestLine lineString
          headerString = getRequestHeaderString requestString
          headers = getHeaderTupelList headerString

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

  parseRequestLine :: String -> RequestLine
  parseRequestLine line = callLineCtor lineElems
    where lineElems = Split.splitOn " " line

  callLineCtor :: [String] -> RequestLine
  callLineCtor [a,b,c] = RequestLine a b c
  callLineCtor param@_ = error $ "Wrong listsize for creating a Requestline." ++ show param

  parseHeaders :: String -> [String]
  parseHeaders headers = Split.splitOn "\n" headers

  getHeaderTupelList :: [String] -> [(String, String)]
  getHeaderTupelList headers = [toTupel $ Split.splitOn ":" h | h <- headers, Prelude.length h > 1]

  toTupel :: [String] -> (String, String)
  toTupel [a,b] = (a,b)
  toTupel [a,b,c] = (a,comb)
    where comb = b ++ ":" ++ c
