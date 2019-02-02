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
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Line.Bot.Data
  ( ChatType(..)
  , Id(..)
  , URL(..)
  , Message(..)
  , ReplyToken(..)
  , ReplyMessageBody(ReplyMessageBody)
  , PushMessageBody(PushMessageBody)
  , MulticastMessageBody(MulticastMessageBody)
  , Profile(..)
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char        (toLower)
import           Data.Maybe       (fromJust)
import           Data.String
import           Data.Text        as T hiding (toLower)
import           GHC.Generics     hiding (to)
import           Servant.API
import           Text.Show

data ChatType = User | Group | Room

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

newtype URL = URL Text
  deriving (Show, Eq, Generic)

instance ToJSON URL
instance FromJSON URL

data Message =
    Text     { text :: Text
             }
  | Sticker  { packageId :: Text
             , stickerId :: Text
             }
  | Image    { originalContentUrl :: URL
             , previewImageUrl    :: URL
             }
  | Video    { originalContentUrl :: URL
             , previewImageUrl    :: URL
             }
  | Audio    { originalContentUrl :: URL
             , duration           :: Int
             }
  | Location { title     :: Text
             , address   :: Text
             , latitude  :: Double
             , longitude :: Double
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
  , constructorTagModifier = fmap toLower
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
