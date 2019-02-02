{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Line.Bot.Webhook.Data
  ( Events(..)
  , Event(..)
  , Message(..)
  , EpochMilli(..)
  , Source(..)
  , Members(..)
  , PostbackEvent(..)
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Foldable
import           Data.List             as L (stripPrefix)
import           Data.Maybe
import           Data.Scientific
import           Data.Text             as T hiding (stripPrefix, toLower)
import           Data.Time             (UTCTime)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)
import           Line.Bot.Data         hiding (Message, Text)


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
                 , timestamp  :: EpochMilli
                 }
  | Follow       { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: EpochMilli
                 }
  | Unfollow     { source    :: Source
                 , timestamp :: EpochMilli
                 }
  | Join         { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: EpochMilli
                 }
  | Leave        { source    :: Source
                 , timestamp :: EpochMilli
                 }
  | MemberJoined { replyToken :: ReplyToken
                 , source     :: Source
                 , timestamp  :: EpochMilli
                 , joined     :: Members
                 }
  | MemberLeft   { source    :: Source
                 , timestamp :: EpochMilli
                 , left      :: Members
                 }
  | Postback     { replyToken :: ReplyToken
                 , source     :: Source
                 , postback   :: PostbackEvent
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
    Text     { messageId :: Text
             , text      :: Text
             }
  | Image    { messageId       :: Text
             , contentProvider :: ContentProvider
             }
  | Video    { messageId       :: Text
             , duration        :: Int
             , contentProvider :: ContentProvider
             }
  | Audio    { messageId       :: Text
             , duration        :: Int
             , contentProvider :: ContentProvider
             }
  | File     { messageId :: Text
             , fileSize  :: Int
             , fileName  :: Text
             }
  | Location { messageId :: Text
             , title     :: Text
             , address   :: Text
             , latitude  :: Double
             , longitude :: Double
             }
  | Sticker  { messageId :: Text
             , packageId :: Text
             , stickerId :: Text
             }
  deriving (Eq, Show, Generic)

messageJSONOptions :: Options
messageJSONOptions = defaultOptions
  { sumEncoding = TaggedObject { tagFieldName = "type" }
  , constructorTagModifier = fmap toLower
  , fieldLabelModifier = \orig ->
      case L.stripPrefix "message" orig of
        Just s  -> fmap toLower s
        Nothing -> orig
  , omitNothingFields = True
  }

instance FromJSON Message where
  parseJSON = genericParseJSON messageJSONOptions


data ContentProvider = ContentProvider
  { originalContentUrl :: Maybe URL
  , previewImageUrl    :: Maybe URL
  }
  deriving (Eq, Show, Generic)

instance FromJSON ContentProvider

newtype EpochMilli = EpochMilli {
  fromEpochMilli :: UTCTime
  -- ^ Acquire the underlying value.
} deriving (Eq, Ord, Read, Show, FormatTime)

instance FromJSON EpochMilli where
  parseJSON = withScientific "EpochMilli" $ \t ->
    pure $ millis t
    where
      millis = EpochMilli
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

data PostbackDateTime =
    PostbackDay Day
  | PostbackLocalTime LocalTime
  | PostbackTimeOfDay TimeOfDay
  deriving (Eq, Show)

instance FromJSON PostbackDateTime where
  parseJSON = withObject "PostbackDateTime" $ \o -> do
    dateTime <- asum
      [ PostbackDay       <$> o .: "date"
      , PostbackLocalTime <$> o .: "datetime"
      , PostbackTimeOfDay <$> o .: "time"
      ]
    return dateTime

data PostbackEvent = PostbackEvent Text PostbackDateTime
  deriving (Eq, Show)

instance FromJSON PostbackEvent where
  parseJSON = withObject "PostbackEvent" $ \o -> do
    postbackData <- o .: "data"
    params       <- o .: "params"
    return $ PostbackEvent postbackData params

