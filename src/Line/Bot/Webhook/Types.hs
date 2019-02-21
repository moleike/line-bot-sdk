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

module Line.Bot.Webhook.Types
  ( ChannelSecret(..)
  , Events(..)
  , Event(..)
  , Message(..)
  , EpochMilli(..)
  , Source(..)
  , Members(..)
  , Postback(..)
  , Beacon(..)
  , BeaconEvent(..)
  , Things(..)
  , ThingsEvent(..)
  , AccountLink(..)
  , AccountLinkResult(..)
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Foldable
import           Data.List             as L (stripPrefix)
import           Data.Maybe
import           Data.Scientific
import           Data.String
import           Data.Text             as T hiding (drop, stripPrefix, toLower)
import           Data.Time             (LocalTime, UTCTime)
import           Data.Time.Calendar    (Day)
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)
import           Line.Bot.Types        hiding (Message, Text)


newtype ChannelSecret = ChannelSecret
  { unChannelSecret :: B.ByteString }

instance IsString ChannelSecret where
  fromString s = ChannelSecret (B.pack s)

data Events = Events
  { destination :: Id User
  , events      :: [Event]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Events

data Event =
    EventMessage      { replyToken :: ReplyToken
                      , message    :: Message
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      }
  | EventFollow       { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      }
  | EventUnfollow     { source    :: Source
                      , timestamp :: EpochMilli
                      }
  | EventJoin         { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      }
  | EventLeave        { source    :: Source
                      , timestamp :: EpochMilli
                      }
  | EventMemberJoined { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      , joined     :: Members
                      }
  | EventMemberLeft   { source    :: Source
                      , timestamp :: EpochMilli
                      , left      :: Members
                      }
  | EventPostback     { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      , postback   :: Postback
                      }
  | EventBeacon       { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      , beacon     :: Beacon
                      }
  | EventAccountLink  { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      , link       :: AccountLink
                      }
  | EventThings       { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      , things     :: Things
                      }
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = genericParseJSON $
    defaultOptions { sumEncoding            = TaggedObject
                     { tagFieldName      = "type"
                     , contentsFieldName = undefined
                     }
                   , constructorTagModifier = (\(x : xs) -> toLower x : xs) . drop 5
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
  { sumEncoding            = TaggedObject
    { tagFieldName      = "type"
    , contentsFieldName = undefined
    }
  , constructorTagModifier = fmap toLower
  , fieldLabelModifier     = \orig ->
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


data Members = Members { members :: [Source] }
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

data Postback = Postback Text PostbackDateTime
  deriving (Eq, Show)

instance FromJSON Postback where
  parseJSON = withObject "Postback" $ \o -> do
    postbackData <- o .: "data"
    params       <- o .: "params"
    return $ Postback postbackData params

data BeaconEvent = Enter | Leave | Banner
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON BeaconEvent where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = fmap toLower
                   , allNullaryToStringTag  = True
                   }

data Beacon = Beacon
   { hwid      :: Text
   , eventType :: BeaconEvent
   , dm        :: Maybe Text
   }
   deriving (Eq, Show, Generic)

instance FromJSON Beacon where
  parseJSON = withObject "Beacon" $ \o -> do
    hwid      <- o .:  "hwid"
    eventType <- o .:  "type"
    dm        <- o .:? "dm"
    return $ Beacon{..}

data AccountLinkResult = Ok | Failed
 deriving (Eq, Show, Generic)

instance FromJSON AccountLinkResult where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = fmap toLower
                   , allNullaryToStringTag  = True
                   }

data AccountLink = AccountLink
   { nonce  :: Text
   , result :: AccountLinkResult
   }
   deriving (Eq, Show, Generic)

instance FromJSON AccountLink

data ThingsEvent = Link | Unlink
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON ThingsEvent where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = fmap toLower
                   , allNullaryToStringTag  = True
                   }

data Things = Things
   { deviceId  :: Text
   , eventType :: ThingsEvent
   }
   deriving (Eq, Show, Generic)

instance FromJSON Things where
  parseJSON = withObject "Things" $ \o -> do
    deviceId  <- o .: "deviceId"
    eventType <- o .: "type"
    return $ Things{..}
