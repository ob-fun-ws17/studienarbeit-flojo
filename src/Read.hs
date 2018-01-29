{-# LANGUAGE OverloadedStrings #-}
module Read (read, readWithPrefix)
where

import Prelude hiding (read)

import qualified Data.ByteString as BS

import Control.Monad
import Control.Exception (catch)

import Response.Error

type FileContents = BS.ByteString
type RequestResult = Either Error FileContents

-- | Read a file and wrap the result into an Either for easier error handling.
read :: (String -> IO BS.ByteString) -> String -> IO RequestResult
read rd path  = catch( do
  result <- rd path
  return $ Right result
  ) handler
  where
    handler :: IOError -> IO RequestResult
    handler ex = do
      return $ Left $ FileDoesNotExist $ show ex -- todo: not all IO errors are not found

readWithPrefix :: (String -> IO BS.ByteString) -> String -> String -> IO RequestResult
readWithPrefix rd prefix = \p -> read rd $ prefix ++ p
