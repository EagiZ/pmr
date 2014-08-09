{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Network.Socket (setSocketOption, SocketOption(..))
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, hClose, hFlush, hIsClosed)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.Chan
import Control.Exception.Base
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import User

type Msg = Either (String, Chan String) String

main :: IO ()
main = withSocketsDo $ do -- withSocketsDo required for Windows usage
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  let tickrate = 64
  let timeout = 500
  socket <- listenOn $ PortNumber port
  setSocketOption socket SendTimeOut timeout
  putStrLn $ "Listening on port " ++ (head args)
  putStrLn $ "Spawning supervisor"
  broadcastChan <- newChan
  messageBox <- newChan
  _ <- forkIO $ spaceFixer broadcastChan
  _ <- forkIO $ supervisor broadcastChan messageBox []
  _ <- forkIO $ tickHandler messageBox (1000000 `div` tickrate)
  sockHandler socket broadcastChan messageBox

spaceFixer :: Chan Msg -> IO ()
spaceFixer chan = do
  eitherMsg <- readChan chan
  case eitherMsg of
    _ -> spaceFixer chan

sockHandler :: Socket -> Chan Msg -> Chan Msg -> IO ()
sockHandler sock broadcastChan messageBox = do
  (clientHandle, _HostName, _PortNumber) <- accept sock
  hSetBuffering clientHandle LineBuffering
  putStrLn "User connected."
  socketChan <- newChan
  _ <- forkIO $ commandProcessor clientHandle broadcastChan messageBox socketChan -- Start commandProcessor in new thread
  sockHandler sock broadcastChan messageBox

tickHandler :: Chan Msg -> Int -> IO ()
tickHandler messageBox sleepTime = do
  threadDelay sleepTime
  writeChan messageBox $ Right ("tick")
  tickHandler messageBox sleepTime 

createUniqueID :: [User] -> [Int] -> Int
createUniqueID [] candidates =
  head candidates
createUniqueID (user@(User userID _ _ _ _ _ _ _ _ _):tl) candidates =
  createUniqueID tl $ filter (/=userID) candidates 

supervisor :: Chan Msg -> Chan Msg -> [User] -> IO()
supervisor broadcastChan messageBox userL = do
  eitherMsg <- readChan messageBox
  case eitherMsg of
    Left (line, chan) -> do
      case (head $ words line) of
        ("connect") -> do
          case decodeJSON $! unwords (tail $ words line) of
            Left err -> do
              putStrLn err
            Right user@(User userID username score xPos yPos xVel yVel acc radius isAlive) -> do
              let newUserID = createUniqueID userL [1..(length userL + 2)]
              let (tempX, tempY) = (80.0 * fromIntegral(newUserID), 400.0) -- TODO: temp solution used for dev purposes
              let tempRadius = 20.0
              let createdUser = User newUserID username 0 tempX tempY 0.0 0.0 acc tempRadius False 
              testPrint user
              testPrint createdUser
              writeChan chan $ encodeJSON createdUser
              supervisor broadcastChan messageBox $ createdUser:userL
        ("refresh") -> do
          writeChan chan $ encodeJSON userL
          supervisor broadcastChan messageBox (userL)
        _ -> do
          putStrLn "SUPERVISOR ERROR: received unkown message in messageBox"
    Right line -> do
      case (head $ words line) of
        ("move") -> do
          case decodeJSON (unwords (tail $ words line)) of
            Left err -> do
              putStrLn err
            Right user -> do
              supervisor broadcastChan messageBox (updateUser userL user)
        ("disconnect") -> do
          case decodeJSON (unwords (tail $ words line)) of
            Left err -> do
              putStrLn err
            Right user -> do
              supervisor broadcastChan messageBox (removeUser userL user)
        ("tick") -> do
          let newUserL = updateUsers userL
          writeChan broadcastChan $ Right $ "refresh " ++ (encodeJSON newUserL)
          supervisor broadcastChan messageBox (newUserL)
        _ -> do
          putStrLn "SUPERVISOR ERROR: received unkown message in messageBox"
        

commandProcessor :: Handle -> Chan Msg -> Chan Msg -> Chan String -> IO ()
commandProcessor clientHandle broadcastChan messageBox socketChan = do
  fromClient <- hGetLine clientHandle
  case (head $ words fromClient) of
    ("connect") -> do
      receiveChannel <- newChan
      writeChan messageBox $ Left (fromClient, receiveChannel)
      line <- readChan receiveChannel
      writeChan socketChan line
      broadcastChan' <- dupChan broadcastChan
      _ <- forkIO $ sendLoop clientHandle socketChan
      _ <- forkIO $ receiveLoop clientHandle broadcastChan' messageBox socketChan -- Start receiveLoop in new thread
      putStrLn "Player connected"
    ("disconnect") -> do
      writeChan messageBox $ Right fromClient
      hFlush clientHandle
      hClose clientHandle
      putStrLn "exiting thread"
      return ()
    ("refresh") -> do
      receiveChannel <- newChan
      writeChan messageBox $ Left (fromClient, receiveChannel)
      line <- readChan receiveChannel
      writeChan socketChan line
    ("move") -> do
      writeChan messageBox $ Right fromClient
    _ -> do
      case decodeJSON fromClient of
        Left err -> do
          putStrLn err
          writeChan socketChan err
        Right user -> do
          testPrint user
          writeChan socketChan (encodeJSON user)
  commandProcessor clientHandle broadcastChan messageBox socketChan

sendLoopDeadLetters :: Chan String -> IO ()
sendLoopDeadLetters chan = do
  _ <- readChan chan
  sendLoopDeadLetters chan

sendLoop :: Handle -> Chan String -> IO ()
sendLoop clientHandle socketChan = do
  line <- readChan socketChan
  result <- try (hPutStrLn clientHandle line) :: IO (Either SomeException ()) -- TODO: this is a bad workaround but will probably suffice for the time being
  case result of
    Left _ ->
      sendLoopDeadLetters socketChan
    Right _ -> do
      hFlush clientHandle
  sendLoop clientHandle socketChan

-- a receive loop that should be created along with a comma+ndProcessor with the same handle
receiveLoop :: Handle -> Chan Msg -> Chan Msg -> Chan String -> IO ()
receiveLoop clientHandle broadcastChan messageBox socketChan = do
  eitherChan <- readChan broadcastChan
  --putStrLn "reading broadcast chan"
  case eitherChan of
    Right line -> do
      --let cmd = words line
      case (head $ words line) of
        ("refresh") -> do
          --putStrLn "receiveLoop refresh writing to socketChan"
          writeChan socketChan $ unwords $ tail $ words line
        _ -> do
          putStrLn "RECEIVELOOP: received unkown command"
    _ -> do
      putStrLn "RECEIVELOOP: received Left (String, Chan String)"
  receiveLoop clientHandle broadcastChan messageBox socketChan

moveCommand :: Handle -> [String] -> IO ()
moveCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

connectCommand :: Handle -> [String] -> IO ()
connectCommand clientHandle cmd = do
  hPutStrLn clientHandle (unwords $ tail cmd) -- TODO: do something valuable

updateUsers :: [User] -> [User]
updateUsers [] = []
updateUsers (users) =
  let updateUsers' :: [User] -> [User] -> [User]
      updateUsers' [] acc = acc
      updateUsers' (user : users) acc =
        updateUsers' users $! ((update user) : acc)
  in
  updateUsers' users []

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
