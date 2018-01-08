module Request.RequestLine (
    RequestLine(RequestLine)
  , ParseError(..)
  , fromString
  , path
  , version
  , method
) where

import Request.Error
import Data.List.Split
import Control.Monad

type ParseMonad = Either ParseError
data RequestLine = RequestLine {m :: String, p :: String, v :: String} deriving (Show, Eq)

method :: RequestLine -> String
method (RequestLine m _ _) = m

path :: RequestLine -> String
path (RequestLine _ p _) = p

version :: RequestLine -> String
version (RequestLine _ _ v) = v

fromString :: String -> ParseMonad RequestLine
fromString line = toRequestLine fields
  where fields = splitOn " " line
        toRequestLine ["GET", p ,"HTTP/1.1"] = Right $ RequestLine "GET" p "HTTP/1.1"
        toRequestLine [m, _, "HTTP/1.1"] = Left $ HttpMethodNotSupported m
        toRequestLine ["GET", _, v] = Left $ HttpVersionNotSupported v
        toRequestLine param@_ = Left $ RequestLineMalformed line
