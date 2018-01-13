 {-# LANGUAGE OverloadedStrings #-}

module Server (
  start
)
where

import Network.Socket
import Data.ByteString.Char8 as BS2
import Network.Socket.ByteString as SockBS
import System.IO as IO
import Response.StatusCode
import Response.Error
import Response.Response as R
import Request.Request
import qualified Read as RD
import Data.Map
import Data.List.Split as Split

defaultConfig = fromList [("contentRoot", "htdocs"), ("port", "8080")]

start :: String -> IO()
start confPath =
   do conf <- RD.read confPath
      case conf of
          Left err -> configurePort defaultConfig
          Right config -> configurePort $ fillConfMap $ BS2.unpack config

fillConfMap :: String -> Map String String
fillConfMap file = fromList $ [getTupel $ lineList line | line <- allLines]
   where allLines = Split.splitOn "\n" file
         getTupel [a,b] = (a, b)
         getTupel list = ("","")
         lineList l = Split.splitOn ":" l

configureRead :: Map String String -> String -> IO (Either Error BS2.ByteString)
configureRead c path = RD.read $ (c ! "contentRoot") ++ path

myRead = configureRead defaultConfig

configurePort :: Map String String -> IO ()
configurePort conf = startServer $ read (conf ! "port")

configuredStart = configurePort defaultConfig

startServer :: PortNumber -> IO ()
startServer port =  do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    IO.putStrLn $ "Binding to socket on port " ++ show port
    bind sock (SockAddrInet port iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do
    conn <- accept sock
    handleConnection conn
    mainLoop sock

handleConnection :: (Socket, SockAddr) -> IO()
handleConnection (sock, _) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering

    request <- parseRequest handle

    file <- myRead $ BS2.unpack $ path request
    case file of
      Left err -> do
        response <- buildErrorResponse err
        sendResponse sock response
      Right fileContent -> sendResponse sock $ (buildOkResponse []) fileContent
    hClose handle

buildErrorResponse :: Error -> IO Response
buildErrorResponse OtherError = do
  responseContent <- myRead "/500.html"
  return $ someFunc (buildNotFoundResponse [("Content-type", ["text/html"])]) responseContent
buildErrorResponse (FileDoesNotExist _) = do
  responseContent <- myRead "/404.html"
  return $ someFunc (buildNotFoundResponse [("Content-type", ["text/html"])]) responseContent

someFunc :: (ByteString -> Response) -> Either Error ByteString -> Response
someFunc responseTemplate readResult =
  case readResult of
    Left _ -> responseTemplate ""
    Right content -> responseTemplate content

sendResponse :: Socket -> Response -> IO Int
sendResponse sock response = SockBS.send sock $ R.toByteString response
