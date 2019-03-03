{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
-- |
-- Module      : Line.Bot.Types
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Types
  ( ChannelToken(..)
  , ChannelSecret(..)
  , ChatType(..)
  , Id(..)
  , MessageId
  , URL(..)
  , Message(..)
  , ReplyToken(..)
  , LinkToken(..)
  , ReplyMessageBody(ReplyMessageBody)
  , PushMessageBody(PushMessageBody)
  , MulticastMessageBody(MulticastMessageBody)
  , Profile(..)
  , QuickReply(..)
  , QuickReplyButton(..)
  , Action(..)
  , ClientCredentials(..)
  , ShortLivedChannelToken(..)
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import           Data.Char             (toLower)
import           Data.List             as L (stripPrefix)
import           Data.Maybe            (fromJust)
import           Data.Monoid           ((<>))
import           Data.String
import           Data.Text             as T hiding (drop, toLower)
import           Data.Text.Encoding
import           GHC.Generics          hiding (to)
import           Servant.API
import           Text.Show
import           Web.FormUrlEncoded    (ToForm (..))

newtype ChannelToken = ChannelToken Text
  deriving (Eq, Show, Generic)

instance FromJSON ChannelToken

instance IsString ChannelToken where
  fromString s = ChannelToken (fromString s)

instance ToHttpApiData ChannelToken where
  toQueryParam (ChannelToken t) = "Bearer " <> t

instance ToForm ChannelToken where
  toForm (ChannelToken t) = [ ("access_token", t) ]

newtype ChannelSecret = ChannelSecret
  { unChannelSecret :: B.ByteString }

instance IsString ChannelSecret where
  fromString s = ChannelSecret (B.pack s)

instance ToHttpApiData ChannelSecret where
  toQueryParam = decodeUtf8 . unChannelSecret

data ChatType = User | Group | Room

-- | ID of a chat user, group or room
data Id :: ChatType -> * where
  UserId  :: Text -> Id User
  GroupId :: Text -> Id Group
  RoomId  :: Text -> Id Room

deriving instance Eq (Id a)
deriving instance Show (Id a)

instance ToHttpApiData (Id a) where
  toQueryParam = \case
    UserId a  -> a
    GroupId a -> a
    RoomId a  -> a

instance ToJSON (Id a) where
  toJSON = String . toQueryParam

instance FromJSON (Id User) where
  parseJSON = withText "Id User" $ return . UserId

instance FromJSON (Id Group) where
  parseJSON = withText "Id Group" $ return . GroupId

instance FromJSON (Id Room) where
  parseJSON = withText "Id Room" $ return . RoomId

type MessageId = Text

newtype URL = URL Text
  deriving (Show, Eq, Generic)

instance ToJSON URL
instance FromJSON URL

data Message =
    MessageText     { text       :: Text
                    , quickReply :: Maybe QuickReply
                    }
  | MessageSticker  { packageId  :: Text
                    , stickerId  :: Text
                    , quickReply :: Maybe QuickReply
                    }
  | MessageImage    { originalContentUrl :: URL
                    , previewImageUrl    :: URL
                    , quickReply         :: Maybe QuickReply
                    }
  | MessageVideo    { originalContentUrl :: URL
                    , previewImageUrl    :: URL
                    , quickReply         :: Maybe QuickReply
                    }
  | MessageAudio    { originalContentUrl :: URL
                    , duration           :: Int
                    , quickReply         :: Maybe QuickReply
                    }
  | MessageLocation { title      :: Text
                    , address    :: Text
                    , latitude   :: Double
                    , longitude  :: Double
                    , quickReply :: Maybe QuickReply
                    }
  deriving (Eq, Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON messageJSONOptions

messageJSONOptions :: Options
messageJSONOptions = defaultOptions
  { sumEncoding            = TaggedObject
    { tagFieldName      = "type"
    , contentsFieldName = undefined
    }
  , constructorTagModifier = fmap toLower . drop 7
  , omitNothingFields      = True
  }

data Profile = Profile
  { displayName   :: Text
  , userId        :: Id User
  , pictureUrl    :: URL
  , statusMessage :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Profile

newtype ReplyToken = ReplyToken Text
  deriving (Eq, Show, Generic)

instance ToJSON ReplyToken
instance FromJSON ReplyToken

data LinkToken = LinkToken { linkToken :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON LinkToken

data ReplyMessageBody = ReplyMessageBody
  { replyToken :: ReplyToken
  , messages   :: [Message]
  }
  deriving (Show, Generic)

instance ToJSON ReplyMessageBody

data PushMessageBody = forall a. PushMessageBody
  { to       :: Id a
  , messages :: [Message]
  }

deriving instance Show (PushMessageBody)

instance ToJSON PushMessageBody where
  toJSON (PushMessageBody {..}) = object
    [ "to"       .= to
    , "messages" .= messages
    ]

data MulticastMessageBody = MulticastMessageBody
  { to       :: [Id User]
  , messages :: [Message]
  }
  deriving (Show, Generic)

instance ToJSON MulticastMessageBody

data QuickReply = QuickReply
  { items :: [QuickReplyButton] }
  deriving (Eq, Show, Generic)

instance ToJSON QuickReply

data QuickReplyButton =
    Action { imageUrl :: Maybe URL
           , action   :: Action
           }
  deriving (Eq, Show, Generic)

quickReplyButtonJSONOptions :: Options
quickReplyButtonJSONOptions = defaultOptions
  { sumEncoding            = TaggedObject
    { tagFieldName      = "type"
    , contentsFieldName = undefined
    }
  , constructorTagModifier = fmap toLower
  , omitNothingFields      = True
  , tagSingleConstructors  = True
  }

instance ToJSON QuickReplyButton where
  toJSON = genericToJSON quickReplyButtonJSONOptions


data Action =
    ActionPostback   { label        :: Text
                     , postbackData :: Text
                     , displayText  :: Text
                     }
  | ActionMessage    { label :: Text
                     , text  :: Text
                     }
  | ActionURI        { label :: Text
                     , uri   :: Text
                     }
  | ActionCamera     { label :: Text
                     }
  | ActionCameraRoll { label :: Text
                     }
  | ActionLocation   { label :: Text
                     }
  deriving (Eq, Show, Generic)

instance ToJSON Action where
  toJSON = genericToJSON actionJSONOptions

actionJSONOptions :: Options
actionJSONOptions = defaultOptions
  { sumEncoding            = TaggedObject
    { tagFieldName      = "type"
    , contentsFieldName = undefined
    }
  , constructorTagModifier = (\(x : xs) -> toLower x : xs) . drop 6
  , omitNothingFields      = True
  , fieldLabelModifier     = \orig ->
      case L.stripPrefix "postback" orig of
        Just s  -> fmap toLower s
        Nothing -> orig
  }

type ChannelId = Id User

data ClientCredentials = ClientCredentials
  { clientId     :: ChannelId
  , clientSecret :: ChannelSecret
  }

instance ToForm ClientCredentials where
  toForm ClientCredentials{..} =
    [ ("grant_type", "client_credentials")
    , ("client_id", toQueryParam clientId)
    , ("client_secret", toQueryParam clientSecret)
    ]

data ShortLivedChannelToken = ShortLivedChannelToken
  { accessToken :: ChannelToken
  , expiresIn   :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON ShortLivedChannelToken where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }
