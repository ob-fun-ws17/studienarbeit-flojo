{-# LANGUAGE ScopedTypeVariables #-}
module Request.ErrorSpec (spec) where

import Request.Error
import Test.Hspec
import Data.ByteString.Char8

spec :: Spec

path = "This is my path"

spec =
  describe "error" $ do
    describe "MalformedRequestLine" $ do
      describe "show" $ do
        it "should contain the parsed path" $
          show (RequestLineMalformed $ pack path) `shouldContain` path
        it "should contain the cause" $
          show (RequestLineMalformed $ pack path) `shouldContain` "is not in format \"<METHOD> <PATH> <VERSION>\""
