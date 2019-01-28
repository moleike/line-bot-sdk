{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleContexts          #-}

module Line.Bot.Webhook.Message
  (Message(..))
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import qualified Data.Text                     as T
                                         hiding ( toLower )
import           GHC.Generics                   ( Generic )

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
