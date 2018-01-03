
module Main where

import Network.Socket
import Data.ByteString.Lazy as BSL
import System.IO as IO
import StatusCode
import Response as R
import Request.Request
port = 8080

okLine = R.ResponseLine "HTTP/1.1" notFound

main :: IO ()
main  = do
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
  run conn
  mainLoop sock

run :: (Socket, SockAddr) -> IO()
run (sock, _) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering

  --parseRequest' handle

  requestString <- parseToString handle

  let request = parseRequest requestString
  IO.putStrLn "Got content:"
  IO.putStrLn $ show request

  IO.putStr "\nSending message\n"
  IO.putStrLn $ show okLine
  send sock $ show okLine ++ "\r\nContent-Length: 3\r\nContent-Type: text/plain\r\n\r\nHi!"

  hClose handle
