{-# LANGUAGE OverloadedStrings #-}

module Read (
  read
)
where
import Prelude hiding (read)
import qualified Data.ByteString as BS
import System.Directory
import Control.Monad
import Control.Exception (catch)

read :: String -> IO (Either IOError BS.ByteString)
read path = catch( do
  result <- BS.readFile path
  return $ Right result
  ) handler
  where
    handler :: IOError -> IO (Either IOError BS.ByteString)
    handler ex = do
      putStrLn $ "Caught exception: " ++ show ex
      return $ Left ex
