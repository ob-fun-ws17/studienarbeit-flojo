{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Request.RequestLineSpec (spec) where

import Request.Error
import Test.Hspec
import Test.QuickCheck
import Data.Either.Unwrap
import Data.ByteString.Char8 as BS
import Request.Internal.Request


testPath = "/my/path"
testMethod = "GET"
testVersion = "HTTP/1.1"
valid = "GET /my/path HTTP/1.1\r"
invalid = testMethod


validLine = fromString $ valid
invalidLine = fromString invalid

justResult :: (Either b0 RequestLine) -> RequestLine
justResult line = either (const $ RequestLine "" "" "") id line
defaultResult = RequestLine "a" "b" "c"

spec :: Spec
spec =
  describe "requestLine" $ do
    describe "fromString" $ do
      it "should parse a valid line" $
        (justResult validLine) `shouldBe` (RequestLine testMethod testPath testVersion)
      it "should not parse an invalid line" $
        (isLeft invalidLine) `shouldBe` True
    describe "path" $ do
      it "should extract the path" $
        (path $ justResult validLine) `shouldBe` testPath
    describe "method" $ do
      it "should extract the method" $
        (method $ justResult validLine) `shouldBe` testMethod
    describe "version" $ do
      it "should extract the version" $
        (version $ justResult validLine) `shouldBe` testVersion
