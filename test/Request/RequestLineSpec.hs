{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Request.RequestLineSpec (spec) where

import qualified Request.RequestLine as RL
import Request.Error
import Test.Hspec
import Test.QuickCheck
import Data.Either.Unwrap
import Data.ByteString.Char8 as BS


path = "/my/path"
get = "GET"
version = "HTTP/1.1"
valid = "GET /my/path HTTP/1.1\r"
invalid = get


validLine = RL.fromString $ valid
invalidLine = RL.fromString invalid

justResult :: (Either b0 RL.RequestLine) -> RL.RequestLine
justResult line = either (const $ RL.RequestLine "" "" "") id line
defaultResult = RL.RequestLine "a" "b" "c"

spec :: Spec
spec =
  describe "requestLine" $ do
    describe "fromString" $ do
      it "should parse a valid line" $
        (justResult validLine) `shouldBe` (RL.RequestLine get path version)
      it "should not parse an invalid line" $
        (isLeft invalidLine) `shouldBe` True
    describe "path" $ do
      it "should extract the path" $
        (RL.path $ justResult validLine) `shouldBe` path
    describe "method" $ do
      it "should extract the method" $
        (RL.method $ justResult validLine) `shouldBe` get
    describe "version" $ do
      it "should extract the version" $
        (RL.version $ justResult validLine) `shouldBe` version
