{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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
  , ChannelId(..)
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
  , BroadcastMessageBody(BroadcastMessageBody)
  , Profile(..)
  , QuickReply(..)
  , QuickReplyButton(..)
  , Action(..)
  , ClientCredentials(..)
  , ShortLivedChannelToken(..)
  , LineDate(..)
  , MessageCount(..)
  , MessageQuota(..)
  , MemberIds(..)
  , JPEG
  , RichMenuSize(..)
  , RichMenuBounds(..)
  , RichMenuArea(..)
  , RichMenu(..)
  , RichMenuResponse(..)
  , RichMenuId(..)
  , RichMenuResponseList(..)
  , RichMenuBulkLinkBody(..)
  , RichMenuBulkUnlinkBody(..)
  )
where

import           Control.Arrow         ((>>>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as LB
import           Data.Char             (toLower)
import           Data.List             as L (stripPrefix)
import           Data.String
import           Data.Text             as T hiding (count, drop, toLower)
import           Data.Text.Encoding
import           Data.Time.Calendar    (Day)
import           Data.Time.Format
import           Data.Typeable
import           GHC.Generics          hiding (to)
import           Network.HTTP.Media    ((//))
import           Servant.API
import           Web.FormUrlEncoded    (ToForm (..))

newtype ChannelToken = ChannelToken { unChannelToken :: Text }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON ChannelToken where
  parseJSON = withText "ChannelToken" $ return . ChannelToken

instance IsString ChannelToken where
  fromString s = ChannelToken (fromString s)

instance ToHttpApiData ChannelToken where
  toHeader (ChannelToken t)     = encodeUtf8 $ "Bearer " <> t
  toQueryParam (ChannelToken t) = t

instance ToForm ChannelToken where
  toForm (ChannelToken t) = [ ("access_token", t) ]

newtype ChannelSecret = ChannelSecret { unChannelSecret :: C8.ByteString }

instance IsString ChannelSecret where
  fromString s = ChannelSecret (C8.pack s)

instance ToHttpApiData ChannelSecret where
  toQueryParam = decodeUtf8 . unChannelSecret

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving (Eq, Show, Generic, NFData)

instance IsString ChannelId where
  fromString s = ChannelId (fromString s)

instance ToHttpApiData ChannelId where
  toQueryParam (ChannelId t) = t

data ChatType = User | Group | Room

-- | ID of a chat user, group or room
data Id :: ChatType -> * where
  UserId  :: Text -> Id 'User
  GroupId :: Text -> Id 'Group
  RoomId  :: Text -> Id 'Room

deriving instance Eq (Id a)
deriving instance Show (Id a)

instance NFData (Id a) where
    rnf (UserId a)  = rnf a
    rnf (GroupId a) = rnf a
    rnf (RoomId a)  = rnf a

instance ToHttpApiData (Id a) where
  toQueryParam = \case
    UserId a  -> a
    GroupId a -> a
    RoomId a  -> a

instance ToJSON (Id a) where
  toJSON = String . toQueryParam

instance FromHttpApiData (Id 'User) where
  parseUrlPiece = pure . UserId

instance FromHttpApiData (Id 'Group) where
  parseUrlPiece = pure . GroupId

instance FromHttpApiData (Id 'Room) where
  parseUrlPiece = pure . RoomId

instance IsString (Id 'User) where
  fromString s = UserId (fromString s)

instance IsString (Id 'Group) where
  fromString s = GroupId (fromString s)

instance IsString (Id 'Room) where
  fromString s = RoomId (fromString s)

instance FromJSON (Id 'User) where
  parseJSON = withText "Id 'User" $ return . UserId

instance FromJSON (Id 'Group) where
  parseJSON = withText "Id 'Group" $ return . GroupId

instance FromJSON (Id 'Room) where
  parseJSON = withText "Id 'Room" $ return . RoomId

type MessageId = Text

newtype URL = URL Text
  deriving (Show, Eq, Generic, NFData)

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
  | MessageFlex     { altText    :: Text
                    , contents   :: Value
                    , quickReply :: Maybe QuickReply
                    }
  deriving (Eq, Show, Generic, NFData)

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
  , userId        :: Text
  , pictureUrl    :: URL
  , statusMessage :: Maybe Text
  }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON Profile

newtype ReplyToken = ReplyToken Text
  deriving (Eq, Show, Generic, NFData)

instance ToJSON ReplyToken
instance FromJSON ReplyToken

newtype LinkToken = LinkToken { linkToken :: Text }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON LinkToken

data ReplyMessageBody = ReplyMessageBody
  { replyToken :: ReplyToken
  , messages   :: [Message]
  }
  deriving (Show, Generic, NFData)

instance ToJSON ReplyMessageBody

data PushMessageBody = forall a. PushMessageBody
  { to       :: Id a
  , messages :: [Message]
  }

deriving instance Show PushMessageBody

instance ToJSON PushMessageBody where
  toJSON PushMessageBody {..} = object
    [ "to"       .= to
    , "messages" .= messages
    ]

data MulticastMessageBody = MulticastMessageBody
  { to       :: [Id 'User]
  , messages :: [Message]
  }
  deriving (Show, Generic, NFData)

instance ToJSON MulticastMessageBody

newtype BroadcastMessageBody = BroadcastMessageBody
  { messages :: [Message] }
  deriving (Show, Generic, NFData)

instance ToJSON BroadcastMessageBody

newtype QuickReply = QuickReply
  { items :: [QuickReplyButton] }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON QuickReply

data QuickReplyButton = QuickReplyButton
  { imageUrl :: Maybe URL
  , action   :: Action
  }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON QuickReplyButton where
  toJSON QuickReplyButton{..} = object
    [ "type"     .= pack "action"
    , "imageUrl" .= imageUrl
    , "action"   .= action
    ]

data Action =
    ActionPostback   { label        :: Text
                     , postbackData :: Text
                     , displayText  :: Text
                     }
  | ActionMessage    { label :: Text
                     , text  :: Text
                     }
  | ActionUri        { label :: Text
                     , uri   :: URL
                     }
  | ActionCamera     { label :: Text
                     }
  | ActionCameraRoll { label :: Text
                     }
  | ActionLocation   { label :: Text
                     }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Action where
  toJSON = genericToJSON actionJSONOptions

instance FromJSON Action where
  parseJSON = genericParseJSON actionJSONOptions

actionJSONOptions :: Options
actionJSONOptions = defaultOptions
  { sumEncoding            = TaggedObject
    { tagFieldName      = "type"
    , contentsFieldName = undefined
    }
  , constructorTagModifier = drop 6 >>> \(x:xs) -> toLower x : xs
  , omitNothingFields      = True
  , fieldLabelModifier     = \x -> maybe x (fmap toLower) $ L.stripPrefix "postback" x
  }

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
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON ShortLivedChannelToken where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }

newtype LineDate = LineDate { unLineDate :: Day } deriving (Eq)

instance Show LineDate where
  show = formatTime defaultTimeLocale "%Y%m%d" . unLineDate

instance ToHttpApiData LineDate where
  toQueryParam = T.pack . show

data MessageCount = MessageCount
  { count  :: Maybe Int
  , status :: String
  } deriving (Eq, Show)

instance FromJSON MessageCount where
  parseJSON = withObject "MessageCount" $ \o -> do
    count  <- o .:? "success"
    status <- o .:  "status"
    return MessageCount{..}

newtype MessageQuota = MessageQuota { totalUsage :: Int }
  deriving (Eq, Show, Generic, NFData)

instance FromJSON MessageQuota

data MemberIds = MemberIds
  { memberIds :: [Id 'User]
  , next      :: Maybe String
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON MemberIds

data JPEG deriving Typeable

instance Accept JPEG where
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender _ = LB.fromStrict

data RichMenuSize = RichMenuSize
  { width  :: Int
  , height :: Int
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON RichMenuSize
instance ToJSON RichMenuSize

data RichMenuBounds = RichMenuBounds
  { x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON RichMenuBounds
instance ToJSON RichMenuBounds

data RichMenuArea = RichMenuArea
  { bounds :: RichMenuBounds
  , action :: Action
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON RichMenuArea
instance ToJSON RichMenuArea

data RichMenu = RichMenu
  { size        :: RichMenuSize
  , selected    :: Bool
  , name        :: Text
  , chatBarText :: Text
  , areas       :: [RichMenuArea]
  } deriving (Eq, Show, Generic, NFData)

instance FromJSON RichMenu
instance ToJSON RichMenu

data RichMenuResponse = RichMenuResponse
  { richMenuId :: Text
  , richMenu   :: RichMenu
  }
  deriving (Show, Eq, Generic, NFData)

instance FromJSON RichMenuResponse where
  parseJSON = withObject "RichMenuResponse" $ \o -> do
    richMenuId <- o .: "richMenuId"
    richMenu   <- parseJSON (Object o)
    return RichMenuResponse{..}

newtype RichMenuId = RichMenuId
  { richMenuId :: Text }
  deriving (Show, Eq, Generic, NFData)

instance FromJSON RichMenuId

instance ToHttpApiData RichMenuId where
  toQueryParam (RichMenuId a) = a

newtype RichMenuResponseList = RichMenuResponseList
  { richmenus :: [RichMenuResponse] }
  deriving (Show, Eq, Generic, NFData)

instance FromJSON RichMenuResponseList

data RichMenuBulkLinkBody = RichMenuBulkLinkBody
  { richMenuId :: Text
  , userIds    :: [Id 'User]
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON RichMenuBulkLinkBody

newtype RichMenuBulkUnlinkBody = RichMenuBulkUnlinkBody
  { userIds :: [Id 'User] }
  deriving (Show, Eq, Generic, NFData)

instance ToJSON RichMenuBulkUnlinkBody
