{-# LANGUAGE OverloadedStrings #-}
-- | The Request module bundles functions and datatypes to parse a Http-Request.
module Request.Request (
      parseRequest
    , path

) where
  import Request.Internal.Request
