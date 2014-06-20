import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

-- Notes:
-- hPutStrLn is used to send data back to the client
-- hGetLine reads data from client

main :: IO ()
main = withSocketsDo $ do -- withSocketsDo required for Windows usage
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  socket <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ (head args)
  sockHandler socket

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (clientHandle, _HostName, _PortNumber) <- accept sock
  hSetBuffering clientHandle NoBuffering
  forkIO $ commandProcessor clientHandle -- start commandProcessor on new thread
  sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor clientHandle = do
  line <- hGetLine clientHandle
  let cmd = words line
  case (head cmd) of
    ("move") -> do
      putStrLn "recieved move command"
      moveCommand clientHandle cmd
    _ -> do
      hPutStrLn clientHandle "Unknown command"
  commandProcessor clientHandle

moveCommand :: Handle -> [String] -> IO ()
moveCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

connectCommand :: Handle -> [String] -> IO ()
connectCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable
  
