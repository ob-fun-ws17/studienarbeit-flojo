-- | Response errors.
module Response.Error (
  Error(..)
) where

data Error = FileDoesNotExist String | OtherError

instance Show Error where
  show (FileDoesNotExist path) = "File " ++ path ++ " does not exist"
  show OtherError = "Request could not be processed due to unknown reasons"
