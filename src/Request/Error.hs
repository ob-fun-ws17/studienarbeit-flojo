module Request.Error (
  ParseError(..)
) where

  data ParseError = RequestLineMalformed String deriving (Eq)

  instance Show ParseError where
    show (RequestLineMalformed line) = "The request line " ++ line ++ " was malformed"
