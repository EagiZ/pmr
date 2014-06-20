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
                   userName :: String,
                   score :: Int,
                   xPos :: Float,
                   yPos :: Float,
                   xVel :: Float,
                   yVel :: Float,
                   radius :: Float
                 } deriving (Show)

-- Instances to convert our type to/from JSON.
instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "userID"
    <*> v .: "userName"
    <*> v .: "score"
    <*> v .: "xPos"
    <*> v .: "yPos"
    <*> v .: "xVel"
    <*> v .: "yVel"
    <*> v .: "radius"
  parseJSON _ = Control.Applicative.empty

instance ToJSON User where
  toJSON (User userID userName score xPos yPos xVel yVel radius) =
    object [ "userID"   .= userID
           , "userName" .= userName
           , "score"    .= score
           , "xPos"     .= xPos
           , "yPos"     .= yPos
           , "xVel"     .= xVel
           , "yVel"     .= yVel
           , "radius"   .= radius
           ]

decodeJSON :: FromJSON a => BS.ByteString -> Either String a
decodeJSON str =
  eitherDecode str

encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON user =
  encode $ user
