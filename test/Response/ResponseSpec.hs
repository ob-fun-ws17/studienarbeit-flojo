{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Response.ResponseSpec (spec) where

import Response.Response
import qualified Response.StatusCode as SC
import Test.Hspec
import Test.QuickCheck
import Data.ByteString

v = "HTTP/1.1"
h = [("Content-Type", ["application/json"])]
c = "This is my response content"

res = Response SC.ok v h c
spec :: Spec
spec = describe "Response" $ do
  describe "getStatusCode"  $ do
    it "should return the status code" $
      getStatusCode res `shouldBe` SC.getStatusCode SC.ok
  describe "getReasonPharse" $ do
    it "should return the reason phrase" $
      getReasonPhrase res `shouldBe` SC.getReasonPhrase SC.ok
  describe "getHeaders" $ do
    it "should return the headers" $
      getHeaders res `shouldBe` h
  describe "getVersion" $ do
    it "should return the version" $
      getVersion res `shouldBe` v
