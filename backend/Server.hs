import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import User

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
  putStrLn "User connected."
  forkIO $ commandProcessor clientHandle -- start commandProcessor on new thread
  sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor clientHandle = do
  fromClient <- hGetLine clientHandle
  let bsFromClient = BS.fromStrict $ C.pack fromClient
  let decoded = decodeJSON bsFromClient :: Either String User
  case decoded of
    Left err -> do
      putStrLn err
      hPutStrLn clientHandle err
    Right user -> do
      testPrint user
      let newUser' = C.unpack $ BS.toStrict $ encodeJSON user
      hPutStrLn clientHandle newUser'
  commandProcessor clientHandle

moveCommand :: Handle -> [String] -> IO ()
moveCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

connectCommand :: Handle -> [String] -> IO ()
connectCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

testPrint :: User -> IO ()
testPrint (User userID userName score xPos yPos xVel yVel radius) =
  putStrLn ("xPos: " ++ show xPos ++ " - yPos: " ++ show yPos)
