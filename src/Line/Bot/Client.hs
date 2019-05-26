{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- Module      : Line.Bot.Client
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Client
  ( Line
  , runLine
  , runLine'
  , withLineEnv
  -- ** Profile
  , getProfile
  -- ** Group
  , getGroupMemberProfile
  , leaveGroup
  , getGroupMemberUserIds
  , getGroupMemberUserIdsS
  -- ** Room
  , getRoomMemberProfile
  , leaveRoom
  , getRoomMemberUserIds
  , getRoomMemberUserIdsS
  -- ** Message
  , replyMessage
  , pushMessage
  , multicastMessage
  , broadcastMessage
  , getContent
  , getPushMessageCount
  , getReplyMessageCount
  , getMulticastMessageCount
  , getBroadcastMessageCount
  , getMessageQuota
  -- ** Account Link
  , issueLinkToken
  -- ** OAuth
  , issueChannelToken
  , revokeChannelToken
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Lazy        as LB
import           Data.Proxy
import           Data.Time.Calendar          (Day)
import           Line.Bot.Internal.Auth      (Auth, mkAuth)
import           Line.Bot.Internal.Endpoints
import           Line.Bot.Types
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Servant.API                 hiding (Stream)
import           Servant.Client
import           Streaming
import qualified Streaming.Prelude           as S

host :: BaseUrl
host = BaseUrl Https "api.line.me" 443 ""

-- | @Line@ is the monad in which bot requests run. Contains the
-- OAuth access token for a channel
type Line = ReaderT ChannelToken ClientM

withLineEnv :: (ClientEnv -> IO a) -> IO a
withLineEnv app = do
  manager <- newManager tlsManagerSettings
  app $ mkClientEnv manager host

-- | Executes a request in the LINE plaform (default)
runLine' :: ClientM a -> IO (Either ServantError a)
runLine' comp = withLineEnv $ \env -> runClientM comp env

-- | Runs a @Line@ computation with the given channel access token
runLine :: Line a -> ChannelToken -> IO (Either ServantError a)
runLine comp = runLine' . runReaderT comp

type LineAuth a = Auth -> ClientM a

type family AddLineAuth a :: * where
  AddLineAuth (LineAuth a) = Line a
  AddLineAuth (a -> b) = a -> AddLineAuth b

class HasLine a where
  addLineAuth :: a -> AddLineAuth a

instance HasLine (LineAuth a) where
  addLineAuth comp = ask >>= \token -> lift $ comp (mkAuth token)

instance HasLine (a -> LineAuth b) where
  addLineAuth comp = addLineAuth . comp

instance HasLine (a -> b -> LineAuth c) where
  addLineAuth comp = addLineAuth . comp

line :: (HasLine (Client ClientM api), HasClient ClientM api)
     => Proxy api -> AddLineAuth (Client ClientM api)
line api = addLineAuth (client api)

getProfile :: Id User -> Line Profile
getProfile = line (Proxy :: Proxy GetProfile)

getGroupMemberProfile :: Id Group -> Id User -> Line Profile
getGroupMemberProfile = line (Proxy :: Proxy GetGroupMemberProfile)

leaveGroup :: Id Group -> Line NoContent
leaveGroup = line (Proxy :: Proxy LeaveGroup)

getGroupMemberUserIds' :: Id Group -> Maybe String -> Line MemberIds
getGroupMemberUserIds' = line (Proxy :: Proxy GetGroupMemberUserIds)

getGroupMemberUserIdsS :: Id Group -> Stream (Of (Id User)) Line ()
getGroupMemberUserIdsS gid = go gid Nothing
  where
    go gid token = do
      MemberIds{..} <- lift $ getGroupMemberUserIds' gid token
      S.each memberIds
      case next of
        Nothing -> return ()
        token'  -> go gid token'

getGroupMemberUserIds :: Id Group -> Line [Id User]
getGroupMemberUserIds = S.toList_ . getGroupMemberUserIdsS

getRoomMemberProfile :: Id Room -> Id User -> Line Profile
getRoomMemberProfile = line (Proxy :: Proxy GetRoomMemberProfile)

leaveRoom :: Id Room -> Line NoContent
leaveRoom = line (Proxy :: Proxy LeaveRoom)

getRoomMemberUserIds' :: Id Room -> Maybe String -> Line MemberIds
getRoomMemberUserIds' = line (Proxy :: Proxy GetRoomMemberUserIds)

getRoomMemberUserIdsS :: Id Room -> Stream (Of (Id User)) Line ()
getRoomMemberUserIdsS gid = go gid Nothing
  where
    go gid token = do
      MemberIds{..} <- lift $ getRoomMemberUserIds' gid token
      S.each memberIds
      case next of
        Nothing -> return ()
        token'  -> go gid token'

getRoomMemberUserIds :: Id Room -> Line [Id User]
getRoomMemberUserIds = S.toList_ . getRoomMemberUserIdsS

replyMessage' :: ReplyMessageBody -> Line NoContent
replyMessage' = line (Proxy :: Proxy ReplyMessage)

replyMessage :: ReplyToken -> [Message] -> Line NoContent
replyMessage a ms = replyMessage' (ReplyMessageBody a ms)

pushMessage' :: PushMessageBody -> Line NoContent
pushMessage' = line (Proxy :: Proxy PushMessage)

pushMessage :: Id a -> [Message] -> Line NoContent
pushMessage a ms = pushMessage' (PushMessageBody a ms)

multicastMessage' :: MulticastMessageBody -> Line NoContent
multicastMessage' = line (Proxy :: Proxy MulticastMessage)

multicastMessage :: [Id User] -> [Message] -> Line NoContent
multicastMessage a ms = multicastMessage' (MulticastMessageBody a ms)

broadcastMessage' :: BroadcastMessageBody -> Line NoContent
broadcastMessage' = line (Proxy :: Proxy BroadcastMessage)

broadcastMessage :: [Message] -> Line NoContent
broadcastMessage = broadcastMessage' . BroadcastMessageBody

getContent :: MessageId -> Line LB.ByteString
getContent = line (Proxy :: Proxy GetContent)

getPushMessageCount' :: LineDate -> Line MessageCount
getPushMessageCount' = line (Proxy :: Proxy GetPushMessageCount)

getPushMessageCount :: Day -> Line (Maybe Int)
getPushMessageCount = fmap count . getPushMessageCount' . LineDate

getReplyMessageCount' :: LineDate -> Line MessageCount
getReplyMessageCount' = line (Proxy :: Proxy GetReplyMessageCount)

getReplyMessageCount :: Day -> Line (Maybe Int)
getReplyMessageCount = fmap count . getReplyMessageCount' . LineDate

getMulticastMessageCount' :: LineDate -> Line MessageCount
getMulticastMessageCount' = line (Proxy :: Proxy GetMulticastMessageCount)

getMulticastMessageCount :: Day -> Line (Maybe Int)
getMulticastMessageCount = fmap count . getMulticastMessageCount' . LineDate

getBroadcastMessageCount' :: LineDate -> Line MessageCount
getBroadcastMessageCount' = line (Proxy :: Proxy GetBroadcastMessageCount)

getBroadcastMessageCount :: Day -> Line (Maybe Int)
getBroadcastMessageCount = fmap count . getBroadcastMessageCount' . LineDate

getMessageQuota' :: Line MessageQuota
getMessageQuota' = line (Proxy :: Proxy GetMessageQuota)

getMessageQuota :: Line Int
getMessageQuota = fmap totalUsage getMessageQuota'

issueLinkToken :: Id User -> Line LinkToken
issueLinkToken = line (Proxy :: Proxy IssueLinkToken)

issueChannelToken' :: ClientCredentials -> ClientM ShortLivedChannelToken
issueChannelToken' = client (Proxy :: Proxy IssueChannelToken)

issueChannelToken :: ChannelId -> ChannelSecret -> ClientM ShortLivedChannelToken
issueChannelToken a b = issueChannelToken' $ ClientCredentials a b

revokeChannelToken :: ChannelToken -> ClientM NoContent
revokeChannelToken = client (Proxy :: Proxy RevokeChannelToken)
