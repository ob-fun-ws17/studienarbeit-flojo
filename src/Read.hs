{-# LANGUAGE OverloadedStrings #-}

module Read (read)
where

import Prelude hiding (read)

import qualified Data.ByteString as BS

import System.Directory

import Control.Monad
import Control.Exception (catch)

import Response.Error

type FileContents = BS.ByteString
type RequestResult = Either Error FileContents

read :: String -> IO RequestResult
read path = catch( do
  cwd <- getCurrentDirectory
  result <- BS.readFile $ cwd ++ path
  return $ Right result
  ) handler
  where
    handler :: IOError -> IO RequestResult
    handler ex = do
      return $ Left $ FileDoesNotExist $ show ex
