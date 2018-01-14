{-# LANGUAGE OverloadedStrings #-}
module Read (read)
where

import Prelude hiding (read)

import qualified Data.ByteString as BS

import Control.Monad
import Control.Exception (catch)

import Response.Error

type FileContents = BS.ByteString
type RequestResult = Either Error FileContents

-- | Read a file and wrap the result into an Either for easier error handling.
read :: String -> IO RequestResult
read path = catch( do
  result <- BS.readFile $ path
  return $ Right result
  ) handler
  where
    handler :: IOError -> IO RequestResult
    handler ex = do
      return $ Left $ FileDoesNotExist $ show ex -- todo: not all IO errors are not found
