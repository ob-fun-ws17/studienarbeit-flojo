{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Request.RequestSpec (spec) where

import Request.Error
import Test.Hspec
import Test.QuickCheck
import Data.Either.Unwrap
import Data.ByteString.Char8 as BS
import Request.Internal.Request as Req

requestPath = "/my/path"
requestGet = "GET"
requestVersion = "HTTP/1.1"
validRequestString = [BS.pack "GET /some/url HTTP/1.1"]
requestHeaders = [("header1", ["someValue"])]

header = BS.pack "header1:value1"
correctHeaderParsed = ("header1", ["value1"])
headerList = [header, BS.pack "header2:value2"]
correctHeaderListParsed = [correctHeaderParsed, ("header2", ["value2"])]

validRequestLine = RequestLine requestGet requestPath requestVersion
request = Request validRequestLine requestHeaders


spec :: Spec
spec =
  describe "request" $ do
    describe "path" $ do
      it "should extract the path" $
        (path $ requestLine request) `shouldBe` requestPath
    describe "method" $ do
      it "should extract the method" $
        (method $ requestLine request) `shouldBe` requestGet
    describe "version" $ do
      it "should extract the version" $
        (version $ requestLine request) `shouldBe` requestVersion
    describe "headers" $ do
      it "should extract the headers" $
        (Req.headers request) `shouldBe` requestHeaders
    describe "parseRequestFromString" $ do
      it "should parse a request correctly from a bytestring" $
        (Req.parseRequestFromString validRequestString) `shouldBe` (Request (RequestLine "GET" "/some/url" "HTTP/1.1") [])
    describe "parseHeaders" $ do
      it "should parse a list of headers correctly" $
        Req.parseHeaders headerList `shouldBe` correctHeaderListParsed
    describe "parseHeader" $ do
      it "should parse one header correctly" $
        Req.parseHeader header `shouldBe` correctHeaderParsed
