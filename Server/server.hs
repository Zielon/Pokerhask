-- | Simple server app
module Main where
 
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
 
main :: IO ()
main = do
  putStrLn "Provide a port number:"
  port <- getLine
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet (read port::PortNumber) iNADDR_ANY
  listen sock 2
  chan <- newChan
  mainLoop sock chan 0
 
type Msg = (Int, String)
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  (soc, addr) <- accept sock
  putStrLn $ "New connection from # " ++ (show addr)

  -- For every incoming connection create a new thread.
  forkIO (runConn (soc, addr) chan msgNum)
  mainLoop sock chan $! msgNum + 1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Hi, what's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
 
    commLine <- dupChan chan
 
    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop
 
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
             "quit" -> hPutStrLn hdl "Bye!"
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle