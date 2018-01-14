{-# LANGUAGE OverloadedStrings #-}
-- | This module contains the datastructure and functions to parse a requestline.
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

-- | Monad for error handling while parsing a request.
type ParseMonad = Either ParseError
-- | The Constructor for creating a requestline. This datatype contains: the http-method, the path and the http-version of the request.
data RequestLine = RequestLine {m :: ByteString, p :: ByteString, v :: ByteString} deriving (Show, Eq)

-- | Function for getting the method.
method :: RequestLine -> ByteString
method (RequestLine m _ _) = m

-- | Function for getting the path.
path :: RequestLine -> ByteString
path (RequestLine _ p _) = p

-- | Function for getting the version.
version :: RequestLine -> ByteString
version (RequestLine _ _ v) = v

-- | Top level function for parsing a byteString to a RequestLine.
fromString :: ByteString -> ParseMonad RequestLine
fromString line = toRequestLine fields
  where fields = Char8.split ' ' line

-- | Pattern matching for finding malformed requests.
toRequestLine :: [ByteString] -> ParseMonad RequestLine
toRequestLine ["GET", p, "HTTP/1.1\r"] = Right $ RequestLine "GET" p "HTTP/1.1"
toRequestLine [m, _, "HTTP/1.1"] = Left $ HttpMethodNotSupported m
toRequestLine ["GET", p, v] = Left $ HttpVersionNotSupported (BS.append (BS.append (BS.append "GET " p) " ") v)
toRequestLine [m, p, v] = Left $ RequestLineMalformed (BS.append (BS.append m p) v)
