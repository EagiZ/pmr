import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.Chan
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import User


type Msg = (String, Chan String) -- TODO: Should probably be 'Either (Chan String) Bool'

-- Notes:
-- hPutStrLn is used to send data back to the client
-- hGetLine reads data from client

main :: IO ()
main = withSocketsDo $ do -- withSocketsDo required for Windows usage
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  let tickrate = 128
  socket <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ (head args)
  putStrLn $ "Spawning supervisor"
  broadcastChan <- newChan
  messageBox <- newChan
  _ <- forkIO $ supervisor broadcastChan messageBox []
  _ <- forkIO $ tickHandler messageBox (1000000 `div` tickrate)
  sockHandler socket broadcastChan messageBox

sockHandler :: Socket -> Chan Msg -> Chan Msg -> IO ()
sockHandler sock broadcastChan messageBox = do
  (clientHandle, _HostName, _PortNumber) <- accept sock
  hSetBuffering clientHandle NoBuffering
  putStrLn "User connected."
  socketChan <- newChan
  _ <- forkIO $ commandProcessor clientHandle broadcastChan messageBox socketChan -- Start commandProcessor in new thread
  sockHandler sock broadcastChan messageBox

tickHandler :: Chan Msg -> Int -> IO ()
tickHandler messageBox sleepTime = do
  threadDelay sleepTime
  trashDummy <- newChan
  writeChan messageBox ("tick", trashDummy)
  tickHandler messageBox sleepTime 

createUniqueID :: [User] -> [Int] -> Int
createUniqueID [] candidates =
  head candidates
createUniqueID (user@(User userID username score xPos yPos xVel yVel acc radius isAlive):tl) candidates =
  createUniqueID tl $ filter (/=userID) candidates 

supervisor :: Chan Msg -> Chan Msg -> [User] -> IO()
supervisor broadcastChan messageBox userL = do
  (line, chan) <- readChan messageBox
  let cmd = words line
  case (head cmd) of
    ("move") -> do
      let playerAsJSONStr = unwords $ tail cmd
      let bsFromClient = BS.fromStrict $ C.pack playerAsJSONStr
      let decoded = decodeJSON bsFromClient :: Either String User
      case decoded of
        Left err -> do
          putStrLn err
        Right user -> do
          let newUserL = updateUser userL user
          supervisor broadcastChan messageBox newUserL
    ("connect") -> do
      let playerAsJSONStr = unwords $ tail cmd
      let bsFromClient = BS.fromStrict $ C.pack playerAsJSONStr
      let decoded = decodeJSON bsFromClient :: Either String User
      case decoded of
        Left err -> do
          putStrLn err
        Right user@(User userID username score xPos yPos xVel yVel acc radius isAlive) -> do
          let newUserID = createUniqueID userL [1..(length userL + 2)]
          let (tempX, tempY) = (80.0 * fromIntegral(newUserID), 400.0) -- TODO: temp solution used for dev purposes
          let tempRadius = 20.0
          let createdUser = User newUserID username 0 tempX tempY 0.0 0.0 acc tempRadius False
          testPrint user
          testPrint createdUser
          let newUserL = createdUser:userL
          let userAsJSON = C.unpack $ BS.toStrict $ encodeJSON createdUser
          writeChan chan userAsJSON
          supervisor broadcastChan messageBox (newUserL)
    ("disconnect") -> do
      -- todo parse user and act accordingly
      let playerAsJSONStr = unwords $ tail cmd
      let bsFromClient = BS.fromStrict $ C.pack playerAsJSONStr
      let decoded = decodeJSON bsFromClient :: Either String User
      case decoded of
        Left err -> do
          putStrLn err
        Right user -> do
          supervisor broadcastChan messageBox (removeUser userL user)
    ("refresh") -> do
      let usersAsJSON = C.unpack $ BS.toStrict $ encodeJSON userL
      writeChan chan usersAsJSON
      supervisor broadcastChan messageBox (userL)
    ("tick") -> do
      let newUserL = updateUsers userL
      let usersAsJSON = C.unpack $ BS.toStrict $ encodeJSON newUserL
      trashDummy <- newChan
      writeChan broadcastChan ("refresh " ++ usersAsJSON, trashDummy)
      supervisor broadcastChan messageBox $ updateUsers userL
    _ -> do
      putStrLn "received message in messageBox"
      supervisor broadcastChan messageBox userL

commandProcessor :: Handle -> Chan Msg -> Chan Msg -> Chan String -> IO ()
commandProcessor clientHandle broadcastChan messageBox socketChan = do
  fromClient <- hGetLine clientHandle
  let cmd = words fromClient
  case (head cmd) of
    ("connect") -> do
      receiveChannel <- newChan
      writeChan messageBox (fromClient, receiveChannel)
      line <- readChan receiveChannel
      writeChan socketChan line
      broadcastChan' <- dupChan broadcastChan
      _ <- forkIO $ sendLoop clientHandle socketChan
      _ <- forkIO $ receiveLoop clientHandle broadcastChan' messageBox socketChan -- Start receiveLoop in new thread
      putStrLn "Player connected"
    ("disconnect") -> do -- TODO
      receiveChannel <- newChan
      writeChan messageBox (fromClient, receiveChannel)
      line <- readChan receiveChannel
      writeChan socketChan line
    ("refresh") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      receiveChannel <- newChan
      writeChan messageBox (fromClient, receiveChannel)
      line <- readChan receiveChannel
      writeChan socketChan line
    ("move") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      ignoreChannel <- newChan
      writeChan messageBox (fromClient, ignoreChannel)
    _ -> do -- TODO
      let bsFromClient = BS.fromStrict $ C.pack fromClient
      let decoded = decodeJSON bsFromClient :: Either String User
      case decoded of
        Left err -> do
          putStrLn err
          writeChan socketChan err
        Right user -> do
          testPrint user
          let newUser = C.unpack $ BS.toStrict $ encodeJSON user
          writeChan socketChan newUser
  commandProcessor clientHandle broadcastChan messageBox socketChan

sendLoop :: Handle -> Chan String -> IO ()
sendLoop clientHandle socketChan = do
  line <- readChan socketChan
  hPutStrLn clientHandle line
  sendLoop clientHandle socketChan

-- a receive loop that should be created along with a comma+ndProcessor with the same handle
receiveLoop :: Handle -> Chan Msg -> Chan Msg -> Chan String -> IO ()
receiveLoop clientHandle broadcastChan messageBox socketChan = do
  (line, _) <- readChan broadcastChan
  let cmd = words line
  case (head cmd) of
    ("refresh") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      -- putStrLn $ unwords $ tail cmd
      writeChan socketChan $ unwords $ tail cmd
    _ -> do
      putStrLn "receiveLoop received unkown command"
      --writeChan messageBox "hello supervisor"
  receiveLoop clientHandle broadcastChan messageBox socketChan

moveCommand :: Handle -> [String] -> IO ()
moveCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

connectCommand :: Handle -> [String] -> IO ()
connectCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

updateUsers :: [User] -> [User]
updateUsers [] = []
updateUsers (user:users) =
  (update user) : updateUsers users

updateUser :: [User] -> User -> [User]
updateUser [] _ = []
updateUser (oldUser@(User oldID _ _ _ _ _ _ _ _ _):users) newUser@(User newID _ _ _ _ _ _ _ _ _) =
  if oldID == newID then
    newUser:users
  else
    oldUser : updateUser users newUser

removeUser :: [User] -> User -> [User]
removeUser [] _ = []
removeUser (thisUser@(User thisID _ _ _ _ _ _ _ _ _):users) targetUser@(User targetID _ _ _ _ _ _ _ _ _) =
  if thisID == targetID then
    users
  else
    thisUser : removeUser users targetUser
      
testPrint :: User -> IO ()
testPrint (User userID username score xPos yPos xVel yVel acc radius isAlive) =
  putStrLn ("xPos: " ++ show xPos ++ " - yPos: " ++ show yPos)
