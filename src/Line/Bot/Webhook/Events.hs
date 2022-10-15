{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
-- Module      : Line.Bot.Webhook.Events
-- Copyright   : (c) Alexandre Moreno, 2019-2021
-- License     : BSD-3-Clause
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Webhook.Events
  ( Events(..)
  , Event(..)
  , Message(..)
  , ContentProvider(..)
  , EpochMilli(..)
  , Source(..)
  , MessageSource(..)
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

import           Control.Arrow         ((>>>))
import           Data.Aeson
import           Data.Char
import           Data.Foldable
import           Data.List             as L (stripPrefix)
import           Data.Text             as T hiding (drop, toLower)
import           Data.Time             (UTCTime)
import           Data.Time.Calendar    (Day)
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)
import           Line.Bot.Types        hiding (Message)

data Events = Events
  { destination :: Id 'User -- ^ 'User ID of a bot that should receive webhook events
  , events      :: [Event] -- ^ List of webhook event objects
  }
  deriving (Show, Generic)

instance FromJSON Events

-- | Events generated on the LINE Platform.
data Event =
    EventMessage      { replyToken    :: ReplyToken
                      , message       :: Message
                      , messageSource :: MessageSource
                      , timestamp     :: EpochMilli
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
  | UnSend            { source     :: Source
                      , timestamp  :: EpochMilli
                      }
  | VideoPlayComplete { replyToken :: ReplyToken
                      , source     :: Source
                      , timestamp  :: EpochMilli
                      }
  deriving (Show, Generic)

instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TaggedObject
      { tagFieldName      = "type"
      , contentsFieldName = undefined
      }
    , constructorTagModifier = drop 5 >>> \(x:xs) -> toLower x : xs
    , fieldLabelModifier = \s -> if s == "messageSource" then "source" else s
    }

data Message =
    MessageText     { messageId :: MessageId
                    , text      :: Text
                    }
  | MessageImage    { messageId       :: MessageId
                    , contentProvider :: ContentProvider
                    }
  | MessageVideo    { messageId       :: MessageId
                    , duration        :: Int
                    , contentProvider :: ContentProvider
                    }
  | MessageAudio    { messageId       :: MessageId
                    , duration        :: Int
                    , contentProvider :: ContentProvider
                    }
  | MessageFile     { messageId :: MessageId
                    , fileSize  :: Int
                    , fileName  :: Text
                    }
  | MessageLocation { messageId :: MessageId
                    , title     :: Maybe Text
                    , address   :: Text
                    , latitude  :: Double
                    , longitude :: Double
                    }
  | MessageSticker  { messageId :: MessageId
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
  , constructorTagModifier = fmap toLower . drop 7
  , fieldLabelModifier     = \x -> maybe x (fmap toLower) $ L.stripPrefix "message" x
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

data Source = forall a. Source (Id a)

deriving instance Show Source
deriving instance Typeable Source

instance FromJSON Source where
  parseJSON = withObject "Source" $ \o -> do
    messageType <- o .: "type"
    case messageType of
      "user"  -> Source . UserId  <$> o .: "userId"
      "group" -> Source . GroupId <$> o .: "groupId"
      "room"  -> Source . RoomId  <$> o .: "roomId"
      _       -> fail ("unknown source: " ++ messageType)

instance ToJSON Source where
  toJSON (Source (UserId a))  = object ["type" .= String "user", "userId" .= a]
  toJSON (Source (GroupId a)) = object ["type" .= String "group", "groupId" .= a]
  toJSON (Source (RoomId a))  = object ["type" .= String "room", "roomId" .= a]

data MessageSource
  = MessageSourceUser (Id 'User)
  | MessageSourceGroup (Id 'Group) (Id 'User)
  | MessageSourceRoom (Id 'Room) (Id 'User)

deriving instance Show MessageSource
deriving instance Typeable MessageSource

instance FromJSON MessageSource where
  parseJSON = withObject "MessageSource" $ \o -> do
    messageType <- o .: "type"
    case messageType of
      "user"  -> MessageSourceUser . UserId  <$> o .: "userId"
      "group" -> MessageSourceGroup <$> (GroupId <$> o .: "groupId") <*> (UserId <$> o .: "userId")
      "room"  -> MessageSourceRoom <$> (RoomId  <$> o .: "roomId") <*> (UserId <$> o .: "userId")
      _       -> fail ("unknown message source: " ++ messageType)

instance ToJSON MessageSource where
  toJSON (MessageSourceUser (UserId a))  = object ["type" .= String "user", "userId" .= a]
  toJSON (MessageSourceGroup (GroupId a) (UserId b)) = object ["type" .= String "group", "groupId" .= a, "userId" .= b]
  toJSON (MessageSourceRoom (RoomId a) (UserId b))  = object ["type" .= String "room", "roomId" .= a, "userId" .= b]

newtype Members = Members { members :: [Source] }
  deriving (Show, Generic)

instance FromJSON Members

data PostbackDateTime =
    PostbackDay Day
  | PostbackLocalTime LocalTime
  | PostbackTimeOfDay TimeOfDay
  deriving (Eq, Show)

instance FromJSON PostbackDateTime where
  parseJSON = withObject "PostbackDateTime" $ \o ->
    asum
      [ PostbackDay       <$> o .: "date"
      , PostbackLocalTime <$> o .: "datetime"
      , PostbackTimeOfDay <$> o .: "time"
      ]

data Postback = Postback Text (Maybe PostbackDateTime)
  deriving (Eq, Show)

instance FromJSON Postback where
  parseJSON = withObject "Postback" $ \o -> do
    postbackData <- o .: "data"
    params       <- o .:? "params"
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
    return Beacon{..}

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
    return Things{..}
