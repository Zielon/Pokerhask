-- | Simple client app
module Main where

import System.IO
import Control.Monad.Fix
import Control.Monad (liftM, when)
import Control.Exception
import Control.Concurrent
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  putStrLn "Provide a port number:"
  port <- getLine
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  start sock

start :: Socket -> IO ()
start sock = do

  hdl <- socketToHandle sock ReadWriteMode

  threadID <- forkIO $ fix $ \loop -> do
                              name <- hGetLine hdl
                              putStrLn $ name
                              loop

  fix $ \loop -> do
        msg <- getLine
        hPutStrLn hdl msg
        loop

  killThread threadID
