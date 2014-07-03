import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Chan
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import User


type Msg = String

-- Notes:
-- hPutStrLn is used to send data back to the client
-- hGetLine reads data from client

main :: IO ()
main = withSocketsDo $ do -- withSocketsDo required for Windows usage
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  socket <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ (head args)
  putStrLn $ "Spawning supervisor"
  broadcastChan <- newChan
  messageBox <- newChan
  _ <- forkIO $ supervisor broadcastChan messageBox
  sockHandler socket broadcastChan messageBox

sockHandler :: Socket -> Chan Msg -> Chan Msg -> IO ()
sockHandler sock broadcastChan messageBox = do
  (clientHandle, _HostName, _PortNumber) <- accept sock
  hSetBuffering clientHandle NoBuffering
  putStrLn "User connected."
  _ <- forkIO $ commandProcessor clientHandle -- Start commandProcessor in new thread
  broadcastChan' <- dupChan broadcastChan
  _ <- forkIO $ recieveLoop clientHandle broadcastChan' messageBox -- Start recieveLoop in new thread
  let broadcast msg = writeChan broadcastChan' msg
  broadcast "Supervisor testing channel"
  sockHandler sock broadcastChan messageBox

supervisor :: Chan Msg -> Chan Msg -> IO()
supervisor broadcastChan messageBox = do
  line <- readChan messageBox
  let cmd = words line
  case (head cmd) of
    ("move") ->
      putStrLn "supervisor: move tbi"
    ("connect") ->
      putStrLn "supervisor: connect tbi"
    ("disconnect") ->
      putStrLn "supervisor: disconnect tbi"
    _ -> 
      putStrLn "recieved message in messageBox"
  let broadcast msg = writeChan broadcastChan msg
  broadcast "Supervisor testing channel"
  supervisor broadcastChan messageBox

commandProcessor :: Handle  -> IO ()
commandProcessor clientHandle  = do
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

-- a recieve loop that should be created along with a commandProcessor with the same handle
recieveLoop :: Handle -> Chan Msg -> Chan Msg -> IO ()
recieveLoop clientHandle broadcastChan messageBox = do
  line <- readChan broadcastChan
  let cmd = words line
  case (head cmd) of
    ("refresh") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      putStrLn "recieveLoop recieved refresh-message"
    _ -> do
      putStrLn "recieveLoop recieved unkown command"
      writeChan messageBox "hello supervisor"
  recieveLoop clientHandle broadcastChan messageBox

moveCommand :: Handle -> [String] -> IO ()
moveCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

connectCommand :: Handle -> [String] -> IO ()
connectCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

testPrint :: User -> IO ()
testPrint (User userID userName score xPos yPos xVel yVel radius) =
  putStrLn ("xPos: " ++ show xPos ++ " - yPos: " ++ show yPos)
