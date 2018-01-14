{-# LANGUAGE OverloadedStrings #-}
-- |The Error Module, containing predefined errors.
-- These can occur, when a Request is parsed.
module Request.Error (
  ParseError(..)
) where

import Data.ByteString.Char8 as BS

-- | Constructors for different kinds of error.
data ParseError =
  -- | Error, when the first line of the request is not correctly formated.
  RequestLineMalformed ByteString
  -- | Error, when the Http-Method of the request is not supported.
  | HttpMethodNotSupported ByteString
  -- | Error, when the Http Version of the request is not supported.
  | HttpVersionNotSupported ByteString
  -- / Error, that occurs, when parsing fails without known reason.
  | UnknownParseError ByteString deriving (Eq)

-- |The message that is show, when the errors occur.
instance Show ParseError where
  show (RequestLineMalformed line) = "The request line is not in format \"<METHOD> <PATH> <VERSION>\", but was: " ++ (BS.unpack line)
  show (HttpMethodNotSupported method) = "Request method not supported: " ++ (BS.unpack method)
  show (HttpVersionNotSupported version) = "Not supported Http version in Request: " ++ (BS.unpack version)
  show (UnknownParseError string) = "An unknown parse error occured."
