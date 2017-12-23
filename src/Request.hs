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
  import qualified Data.ByteString.Char8 as CHAR8
  import Data.Char as Char

  data RequestLine = RequestLine {m :: String, p :: String, v :: String} deriving (Show)
  data Request = Request {requestLine :: RequestLine, h::[String]} deriving (Show)

  method :: RequestLine -> String
  path :: RequestLine -> String
  version :: RequestLine -> String

  method (RequestLine m _ _) = m
  path (RequestLine _ p _) = p
  version (RequestLine _ _ v) = v

  method' :: Request -> String
  path' :: Request -> String
  version' :: Request -> String
  headers' :: Request -> [String]

  method' (Request line _) = method line
  path' (Request line _) = path line
  version' (Request line _) = version line
  headers' (Request _ h) = h

  parseRequest :: String ->  Request
  parseRequest requestString =
    Request line ["d","e"]
    where lineString = getRequestLineString requestString
          line = parseRequestLine lineString

  parseToString :: Handle -> IO  String
  parseToString handle =
    do r1 <- BSL.hGet handle 1
       rRest <- BSL.hGetNonBlocking handle 1024
       return $ show $ BSL.append r1 rRest

  getRequestLineString :: String -> String
  getRequestLineString request = Prelude.head $ Split.splitOn "\\r\\n" request

  parseRequestLine :: String -> RequestLine
  parseRequestLine line = callLineCtor lineElems
    where lineElems = Split.splitOn " " line

  callLineCtor :: [String] -> RequestLine
  callLineCtor [a,b,c] = RequestLine a b c
  callLineCtor param@_ = error $ "Wrong listsize for creating a Requestline." ++ show param

  parseHeaders :: String -> [String]
  parseHeaders headers = Split.splitOn "\n" headers
