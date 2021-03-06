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

type ReadResult = Either Error ByteString

start :: String -> IO()
start confPath =
   do conf <- RD.read (\p -> BS2.readFile p) confPath
      case conf of
          Left err -> do
            IO.putStrLn $ "Unable to read config, started with default config"
            startServer port readWithCRoot
              where
                port = 8080
                readWithCRoot = RD.readWithPrefix (\p -> BS2.readFile p) "htdocs"
          Right config -> do
            BS2.putStrLn $ BS2.append "Started with config:\n\n" config
            let parsedConfig = fillConfMap $ BS2.unpack config
            startServer (port parsedConfig) (readWithCRoot parsedConfig)
               where
                     port cfg = getPort cfg
                     readWithCRoot cfg = RD.readWithPrefix (\p -> BS2.readFile p) $ getContentRoot cfg

fillConfMap :: String -> Map String String
fillConfMap file = fromList $ [getTupel $ lineList line | line <- allLines]
   where allLines = Split.splitOn "\n" file
         getTupel [a,b] = (a, b)
         getTupel list = ("","")
         lineList l = Split.splitOn ":" l


getPort :: Map String String -> PortNumber
getPort conf = read $ conf ! "port"

getContentRoot :: Map String String -> String
getContentRoot conf = conf ! "contentRoot"

startServer :: PortNumber
                -> (String -> IO ReadResult)
                -> IO ()
startServer port rd =  do
    sock <- socket AF_INET Stream 0    -- create socket

    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    IO.putStrLn $ "Started on port " ++ show port
    bind sock (SockAddrInet port iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock rd


mainLoop :: Socket -> (String -> IO ReadResult) -> IO()
mainLoop sock rd = do
    conn <- accept sock
    handleConnection conn rd
    mainLoop sock rd

handleConnection :: (Socket, SockAddr) -> (String -> IO ReadResult) -> IO()
handleConnection (sock, _) rd = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering

    request <- parseRequest handle

    file <- rd $ BS2.unpack $ path $ requestLine request
    case file of
      Left err -> buildErrorResponse err rd >>= sendResponse sock
      Right fileContent -> sendResponse sock $ (buildOkResponse []) fileContent
    hClose handle

-- | Build a response resulting in a non-success status code.
buildErrorResponse :: Error -> (String -> IO ReadResult) -> IO Response
buildErrorResponse OtherError rd = do
  responseContent <- rd "/500.html"
  return $ withFileContent (buildNotFoundResponse [("Content-type", ["text/html"])]) responseContent
buildErrorResponse (FileDoesNotExist _) rd = do
  responseContent <- rd "/404.html"
  return $ withFileContent (buildNotFoundResponse [("Content-type", ["text/html"])]) responseContent

-- | Write the content to the response. When file reading faulted, default to empty string.
withFileContent :: (ByteString -> Response) -> ReadResult -> Response
withFileContent responseTemplate readResult =
  case readResult of
    Left _ -> responseTemplate ""
    Right content -> responseTemplate content

-- | Send the response.
sendResponse :: Socket -> Response -> IO Int
sendResponse sock response = SockBS.send sock $ R.toByteString response
