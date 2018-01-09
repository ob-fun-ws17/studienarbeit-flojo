{-# LANGUAGE OverloadedStrings #-}

module Request.Error (
  ParseError(..)
) where

import Data.ByteString.Char8 as BS

data ParseError = RequestLineMalformed ByteString | HttpMethodNotSupported ByteString | HttpVersionNotSupported ByteString deriving (Eq)

instance Show ParseError where
  show (RequestLineMalformed line) = "The request line is not in format \"<METHOD> <PATH> <VERSION>\", but was: " ++ (BS.unpack line)
  show (HttpMethodNotSupported method) = "Request method not supported: " ++ (BS.unpack method)
  show (HttpVersionNotSupported version) = "Not supported Http version in Request: " ++ (BS.unpack version)
