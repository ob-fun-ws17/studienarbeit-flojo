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
          (show (RequestLineMalformed path)) `shouldContain` (unpack path)
        it "should contain the cause" $
          (show $ RequestLineMalformed path) `shouldContain` "is not in format \"<METHOD> <PATH> <VERSION>\""
