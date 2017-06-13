-- | Simple client app
module Main where

import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    start "localhost" 4545

start :: String -> Int -> IO ()
start host port = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  massanger sock

massanger :: Socket -> IO ()
massanger sock = do
  msg <- getLine
  sendAll sock $ C.pack msg

  -- recived <- recv sock 4242
  -- putStrLn $ C.unpack recived

  massanger sock