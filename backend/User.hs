{-# LANGUAGE OverloadedStrings #-}

module User where

import Data.Aeson
import Data.Text

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import GHC.Generics


data User = User { userID :: Int,
                   username :: String,
                   score :: Int,
                   xPos :: Float,
                   yPos :: Float,
                   xVel :: Float,
                   yVel :: Float,
                   acc :: Float,
                   radius :: Float,
                   isAlive :: Bool
                 } deriving (Show)

-- Instances to convert our type to/from JSON.
instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "userID"
    <*> v .: "username"
    <*> v .: "score"
    <*> v .: "xPos"
    <*> v .: "yPos"
    <*> v .: "xVel"
    <*> v .: "yVel"
    <*> v .: "acc"
    <*> v .: "radius"
    <*> v .: "isAlive"
  parseJSON _ = Control.Applicative.empty

instance ToJSON User where
  toJSON (User userID username score xPos yPos xVel yVel acc radius isAlive) =
    object [ "userID"   .= userID
           , "username" .= username
           , "score"    .= score
           , "xPos"     .= xPos
           , "yPos"     .= yPos
           , "xVel"     .= xVel
           , "yVel"     .= yVel
           , "acc"     .= acc
           , "radius"   .= radius
           , "isAlive"   .= isAlive
           ]

update :: User -> User
update (User userID username score xPos yPos xVel yVel acc radius isAlive) =
  User userID username score (xPos+xVel) (yPos+yVel) (xVel*acc) (yVel*acc) acc radius isAlive

decodeJSON :: FromJSON a => BS.ByteString -> Either String a
decodeJSON str =
  eitherDecode str

encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON user =
  encode $ user
