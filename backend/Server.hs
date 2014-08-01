import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO, myThreadId)
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
  socket <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ (head args)
  putStrLn $ "Spawning supervisor"
  broadcastChan <- newChan
  messageBox <- newChan
  _ <- forkIO $ supervisor broadcastChan messageBox []
  sockHandler socket broadcastChan messageBox

sockHandler :: Socket -> Chan Msg -> Chan Msg -> IO ()
sockHandler sock broadcastChan messageBox = do
  (clientHandle, _HostName, _PortNumber) <- accept sock
  hSetBuffering clientHandle NoBuffering
  putStrLn "User connected."
  _ <- forkIO $ commandProcessor clientHandle messageBox -- Start commandProcessor in new thread
  broadcastChan' <- dupChan broadcastChan
  _ <- forkIO $ recieveLoop clientHandle broadcastChan' messageBox -- Start recieveLoop in new thread
  sockHandler sock broadcastChan messageBox

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
          let usersAsJSON = C.unpack $ BS.toStrict $ encodeJSON newUserL
          writeChan chan usersAsJSON
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
    _ -> do
      putStrLn "recieved message in messageBox"
      supervisor broadcastChan messageBox userL

commandProcessor :: Handle -> Chan Msg  -> IO ()
commandProcessor clientHandle messageBox  = do
  fromClient <- hGetLine clientHandle
  let cmd = words fromClient
  case (head cmd) of
    ("connect") -> do
      recieveChannel <- newChan
      writeChan messageBox (fromClient, recieveChannel)
      line <- readChan recieveChannel
      hPutStrLn clientHandle line
    ("disconnect") -> do -- TODO
      recieveChannel <- newChan
      writeChan messageBox (fromClient, recieveChannel)
      line <- readChan recieveChannel
      hPutStrLn clientHandle line
    ("refresh") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      recieveChannel <- newChan
      writeChan messageBox (fromClient, recieveChannel)
      line <- readChan recieveChannel
      hPutStrLn clientHandle line
    ("move") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      recieveChannel <- newChan
      writeChan messageBox (fromClient, recieveChannel)
      line <- readChan recieveChannel
      hPutStrLn clientHandle line
    _ -> do -- TODO
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
  commandProcessor clientHandle messageBox

-- a recieve loop that should be created along with a commandProcessor with the same handle
recieveLoop :: Handle -> Chan Msg -> Chan Msg -> IO ()
recieveLoop clientHandle broadcastChan messageBox = do
  (line, chan) <- readChan broadcastChan
  let cmd = words line
  case (head cmd) of
    ("refresh") -> do
      -- TODO: parse tail cmd (which should be JSON-array containing User objects) etc.
      putStrLn "recieveLoop recieved refresh-message"
    _ -> do
      putStrLn "recieveLoop recieved unkown command"
      --writeChan messageBox "hello supervisor"
  recieveLoop clientHandle broadcastChan messageBox

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
  case oldID of
    newID ->
      newUser:users
    _ ->
      oldUser : updateUser users newUser

removeUser :: [User] -> User -> [User]
removeUser [] _ = []
removeUser (thisUser@(User thisID _ _ _ _ _ _ _ _ _):users) targetUser@(User targetID _ _ _ _ _ _ _ _ _) =
  case thisID of
    targetID ->
      users
    _ ->
      thisUser : removeUser users targetUser
      
testPrint :: User -> IO ()
testPrint (User userID username score xPos yPos xVel yVel acc radius isAlive) =
  putStrLn ("xPos: " ++ show xPos ++ " - yPos: " ++ show yPos)
