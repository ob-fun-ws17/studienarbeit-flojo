module Request.Error (
  ParseError(..)
) where

data ParseError = RequestLineMalformed String | HttpMethodNotSupported String | HttpVersionNotSupported String deriving (Eq)

instance Show ParseError where
  show (RequestLineMalformed line) = "The request line " ++ line ++ " is not in format \"<METHOD> <PATH> <VERSION>\""
  show (HttpMethodNotSupported method) = "Request method " ++ method ++ " not supported"
  show (HttpVersionNotSupported version) = "Http version " ++ version ++ " not supported"
