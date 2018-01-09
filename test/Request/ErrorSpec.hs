{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request.ErrorSpec (spec) where

import Request.Error
import Test.Hspec
import Data.ByteString.Char8

spec :: Spec
spec =
  describe "error" $ do
    describe "MalformedRequestLine" $ do
      describe "show" $ do
        let path = "This is my path"
        it "should contain the parsed path" $
          RequestLineMalformed path `shouldContain` path
        it "should contain the cause" $
          RequestLineMalformed pack path `shouldContain` "is not in format \"<METHOD> <PATH> <VERSION>\""
