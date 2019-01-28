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
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( fromJust )
import           Data.String
import           Data.Text                     as T
                                         hiding ( toLower )
import           GHC.Generics            hiding ( to )
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

data Message =
    Text     { text :: String
             }
  | Sticker  { packageId :: String
             , stickerId :: String
             }
  | Image    { originalContentUrl :: String
             , previewImageUrl    :: String
             }
  | Video    { originalContentUrl :: String
             , previewImageUrl    :: String
             }
  | Audio    { originalContentUrl :: String
             , duration           :: Int
             }
  | Location { title     :: String
             , address   :: String
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
  { displayName   :: String
  , userId        :: Id User
  , pictureUrl    :: String
  , statusMessage :: Maybe String
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
