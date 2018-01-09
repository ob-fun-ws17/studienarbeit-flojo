{-# LANGUAGE OverloadedStrings #-}
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
import Data.ByteString as BS
import Data.ByteString.Char8 as Char8

type ParseMonad = Either ParseError
data RequestLine = RequestLine {m :: ByteString, p :: ByteString, v :: ByteString} deriving (Show, Eq)

method :: RequestLine -> ByteString
method (RequestLine m _ _) = m

path :: RequestLine -> ByteString
path (RequestLine _ p _) = p

version :: RequestLine -> ByteString
version (RequestLine _ _ v) = v

fromString :: ByteString -> ParseMonad RequestLine
fromString line = toRequestLine fields
  where fields = Char8.split ' ' line

toRequestLine :: [ByteString] -> ParseMonad RequestLine
toRequestLine ["GET", p, "HTTP/1.1\r"] = Right $ RequestLine "GET" p "HTTP/1.1"
toRequestLine [m, _, "HTTP/1.1"] = Left $ HttpMethodNotSupported m
toRequestLine ["GET", p, v] = Left $ HttpVersionNotSupported (BS.append (BS.append (BS.append "GET " p) " ") v)
toRequestLine [m, p, v] = Left $ RequestLineMalformed (BS.append (BS.append m p) v)

  {-where fields = BS.split " " line
        toRequestLine fields
            | fields == ["GET", p ,"HTTP/1.1"] = Right $ RequestLine "GET" p "HTTP/1.1"
            | fields == [m, _, "HTTP/1.1"] = Left $ HttpMethodNotSupported m
            | fields == ["GET", _, v] = Left $ HttpVersionNotSupported v
            | otherwiese = Left $ RequestLineMalformed line-}
