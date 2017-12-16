
module Main where

import Network.Socket
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import System.IO as IO

data HttpStatusCode = Ok | NotFound
  deriving(Show, Enum)

code :: HttpStatusCode -> Int
reasonPhrase :: HttpStatusCode -> [Char]

code Ok = 200
code NotFound = 404
reasonPhrase Ok = "OK"
reasonPhrase NotFound = "NOT FOUND"

data ResponseLine = ResponseLine String HttpStatusCode

instance Show ResponseLine where
  show ( ResponseLine version status) = version ++ " " ++ show (code status) ++ " " ++ reasonPhrase status

okLine = ResponseLine "HTTP/1.1" Ok

port = 8080

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
  IO.putStrLn "Got content:"
  parseRequest handle

  IO.putStr "\nSending message\n"
  IO.putStrLn $ show okLine
  send sock $ show okLine ++ "\r\nContent-Length: 3\r\nContent-Type: text/plain\r\n\r\nHi!"

  hClose handle

parseRequest handle =
  do
    r1 <- BSL.hGet handle 1
    rRest <- BSL.hGetNonBlocking handle 1024
    IO.putStrLn (show (BSL.append r1 rRest))
