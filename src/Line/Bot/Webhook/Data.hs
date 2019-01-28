{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleContexts          #-}

module Line.Bot.Webhook.Data
  ( Events(..)
  , Event(..)
  , Message(..)
  , Timestamp(..)
  , Source(..)
  , Members(..)
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                     as T
                                         hiding ( toLower )
import           Data.Time                      ( UTCTime )
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Line.Bot.Data           hiding ( Message )


data Events =
  Events { destination :: Id User
         , events      :: [Event]
         }
  deriving (Eq, Show, Generic)

instance FromJSON Events

data Event =
    Message      { replyToken :: ReplyToken
                 , message    :: Message
                 , source     :: Source
                 , timestamp  :: Timestamp
                 }
  | Follow       { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: Timestamp
                 }
  | Unfollow     { source    :: Source
                 , timestamp :: Timestamp
                 }
  | Join         { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: Timestamp
                 }
  | Leave        { source    :: Source
                 , timestamp :: Timestamp
                 }
  | MemberJoined { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: Timestamp
                 , joined     :: Members
                 }
  | MemberLeft   { source    :: Source
                 , timestamp :: Timestamp
                 , left      :: Members
                 }
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = genericParseJSON eventJSONOptions

eventJSONOptions :: Options
eventJSONOptions = defaultOptions
  { sumEncoding            = TaggedObject {tagFieldName = "type"}
  , constructorTagModifier = \(x : xs) -> (toLower x) : xs
  }

data Message =
    Text     { messageId :: String
             , text      :: String
             }
  | Image    { messageId          :: String
             , originalContentUrl :: Maybe String
             , previewImageUrl    :: Maybe String
             }
  | Video    { messageId          :: String
             , duration           :: Int
             , originalContentUrl :: Maybe String
             , previewImageUrl    :: Maybe String
             }
  | Audio    { messageId          :: String
             , duration           :: Int
             , originalContentUrl :: Maybe String
             }
  | File     { messageId :: String
             , fileSize  :: Int
             , fileName  :: String
             }
  | Location { messageId :: String
             , title     :: String
             , address   :: String
             , latitude  :: Double
             , longitude :: Double
             }
  | Sticker  { messageId :: String
             , packageId :: String
             , stickerId :: String
             }
  deriving (Eq, Show, Generic)

messageJSONOptions :: Options
messageJSONOptions = defaultOptions
  { sumEncoding = TaggedObject { tagFieldName = "type" }
  , constructorTagModifier = fmap toLower
  , fieldLabelModifier = \orig ->
      case stripPrefix "message" orig of
        Just s  -> fmap toLower s
        Nothing -> orig
  , omitNothingFields = True
  }

instance FromJSON Message where
  parseJSON = genericParseJSON messageJSONOptions

newtype Timestamp = Timestamp {
  fromMilliseconds :: UTCTime
  -- ^ Acquire the underlying value.
} deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance FromJSON Timestamp where
  parseJSON = withScientific "Timestamp" $ \t ->
    pure $ millis t
    where
      millis = Timestamp
             . posixSecondsToUTCTime
             . fromRational
             . toRational
             . (/ 1000)


data Source =
    SourceUser (Id User)
  | SourceGroup (Id Group) (Maybe (Id User))
  | SourceRoom (Id Room) (Maybe (Id User))
  deriving (Eq, Show)

instance FromJSON Source where
  parseJSON = withObject "Source" $ \o -> do
    messageType <- o .: "type"
    case messageType of
      "user"  -> SourceUser  <$> o .: "userId"
      "group" -> SourceGroup <$> o .: "groupId" <*> o .:? "userId"
      "room"  -> SourceRoom  <$> o .: "roomId"  <*> o .:? "userId"
      _       -> fail ("unknown source: " ++ messageType)


data Members =
  Members { members :: [Source] }
  deriving (Eq, Show, Generic)

instance FromJSON Members

