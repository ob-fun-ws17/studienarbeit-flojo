 {-# LANGUAGE OverloadedStrings #-}

module Server (
  configuredStart
)
where
  import Network.Socket
  import Data.ByteString.Lazy as BSL
  import Data.ByteString.Char8 as BS2
  import Network.Socket.ByteString as SockBS
  import System.IO as IO
  import Response.StatusCode
  import Response.Error
  import Response.Response as R
  import Request.Request
  import qualified Read as RD
  import Data.Map

  config = fromList [("contentRoot", "/home/osboxes"), ("port", "8080")]
  configureRead :: Map String String -> String -> IO (Either Error BS2.ByteString)
  configureRead c path = RD.read $ (c ! "contentRoot") ++ path

  myRead = configureRead config

  configurePort :: Map String String -> IO ()
  configurePort conf = start $ read (conf ! "port")

  configuredStart = configurePort config

  start :: PortNumber -> IO ()
  start port =  do
        sock <- socket AF_INET Stream 0    -- create socket
        setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
        IO.putStrLn $ "Binding to socket on port " ++ show port
        bind sock (SockAddrInet port iNADDR_ANY)   -- listen on TCP port 4242.
        listen sock 2                              -- set a max of 2 queued connections
        mainLoop sock

  mainLoop :: Socket -> IO()
  mainLoop sock = do
    conn <- accept sock
    IO.putStrLn "Accepting connection"
    handleConnection conn
    mainLoop sock

  handleConnection :: (Socket, SockAddr) -> IO()
  handleConnection (sock, _) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering

    request <- parseRequest handle
    IO.putStrLn "Got content:"
    --IO.putStrLn $ show request

    IO.putStr "\nSending message\n"

    file <- myRead $ BS2.unpack $ path request
    case file of
      Left err -> sendResponse "HTTP/1.1 404 NOT FOUND\r\nContent-Length: 0\r\n\r\n"
        where sendResponse response = SockBS.send sock response
      Right c -> sendResponse $ toByteString $ Response ok "HTTP/1.1" [] c
        where sendResponse response = SockBS.send sock response
    hClose handle
