{-# LANGUAGE ScopedTypeVariables #-}
module Request.RequestLineSpec (spec) where

import qualified Request.RequestLine as RL
import Request.Error
import Test.Hspec
import Test.QuickCheck
import Data.Either.Unwrap


path = "/my/path"
get = "GET"
version = "HTTP/1.1"
valid = get ++ " " ++ path ++ " " ++ version
invalid = get


validLine = (fromRight . RL.fromString) valid
invalidLine = (fromLeft . RL.fromString) invalid

spec :: Spec
spec =
  describe "requestLine" $ do
    describe "fromString" $ do
      it "should parse a valid line" $
        validLine `shouldBe` (RL.RequestLine get path version)
      it "should not parse an invalid line" $
        invalidLine `shouldBe` (RequestLineMalformed invalid)
    describe "path" $ do
      it "should extract the path" $
        RL.path validLine `shouldBe` path
    describe "method" $ do
      it "should extract the method" $
        RL.method validLine `shouldBe` get
    describe "version" $ do
      it "should extract the version" $
         RL.version validLine `shouldBe` version
