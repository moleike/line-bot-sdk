{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  -- ** Room
  , getRoomMemberProfile
  , leaveRoom
  -- ** Message
  , replyMessage
  , pushMessage
  , multicastMessage
  , getContent
  , getPushMessageCount
  , getReplyMessageCount
  , getMulticastMessageCount
  -- ** Account Link
  , issueLinkToken
  -- ** OAuth
  , issueChannelToken
  , revokeChannelToken
  )
where

import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.ByteString.Lazy        (ByteString)
import           Data.Proxy
import           Data.Time.Calendar          (Day)
import           Line.Bot.Internal.Auth
import           Line.Bot.Internal.Endpoints
import           Line.Bot.Types
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Servant.API
import           Servant.Client

host :: BaseUrl
host = BaseUrl Https "api.line.me" 443 ""

-- | @Line@ is the monad in which bot requests run. Contains the
-- OAuth access token for a channel
type Line = ReaderT Auth ClientM

withLineEnv :: (ClientEnv -> IO a) -> IO a
withLineEnv app = do
  manager <- newManager tlsManagerSettings
  app $ mkClientEnv manager host

-- | Executes a request in the LINE plaform (default)
runLine' :: ClientM a -> IO (Either ServantError a)
runLine' comp = withLineEnv $ \env -> runClientM comp env

-- | Runs a @Line@ computation with the given channel access token
runLine :: Line a -> ChannelToken -> IO (Either ServantError a)
runLine comp = runLine' . runReaderT comp . mkAuth

getProfile :: Id User -> Line Profile
getProfile a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetProfile) auth a

getGroupMemberProfile :: Id Group -> Id User -> Line Profile
getGroupMemberProfile a b = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetGroupMemberProfile) auth a b

leaveGroup :: Id Group -> Line NoContent
leaveGroup a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy LeaveGroup) auth a

getRoomMemberProfile :: Id Room -> Id User -> Line Profile
getRoomMemberProfile a b = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetRoomMemberProfile) auth a b

leaveRoom :: Id Room -> Line NoContent
leaveRoom a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy LeaveRoom) auth a

replyMessage' :: ReplyMessageBody -> Line NoContent
replyMessage' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy ReplyMessage) auth a

replyMessage :: ReplyToken -> [Message] -> Line NoContent
replyMessage a ms = replyMessage' (ReplyMessageBody a ms)

pushMessage' :: PushMessageBody -> Line NoContent
pushMessage' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy PushMessage) auth a

pushMessage :: Id a -> [Message] -> Line NoContent
pushMessage a ms = pushMessage' (PushMessageBody a ms)

multicastMessage' :: MulticastMessageBody -> Line NoContent
multicastMessage' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy MulticastMessage) auth a

multicastMessage :: [Id User] -> [Message] -> Line NoContent
multicastMessage a ms = multicastMessage' (MulticastMessageBody a ms)

-- | TODO: this should use a streaming library for constant memory usage over large data
getContent :: MessageId -> Line ByteString
getContent a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetContent) auth a

getPushMessageCount' :: LineDate -> Line MessageCount
getPushMessageCount' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetPushMessageCount) auth a

getPushMessageCount :: Day -> Line (Maybe Int)
getPushMessageCount = fmap count . getPushMessageCount' . LineDate

getReplyMessageCount' :: LineDate -> Line MessageCount
getReplyMessageCount' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetReplyMessageCount) auth a

getReplyMessageCount :: Day -> Line (Maybe Int)
getReplyMessageCount = fmap count . getReplyMessageCount' . LineDate

getMulticastMessageCount' :: LineDate -> Line MessageCount
getMulticastMessageCount' a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy GetMulticastMessageCount) auth a

getMulticastMessageCount :: Day -> Line (Maybe Int)
getMulticastMessageCount = fmap count . getMulticastMessageCount' . LineDate

issueLinkToken :: Id User -> Line LinkToken
issueLinkToken a = ask >>= \auth ->
  lift $ client (Proxy :: Proxy IssueLinkToken) auth a

issueChannelToken :: ClientCredentials -> ClientM ShortLivedChannelToken
issueChannelToken = client (Proxy :: Proxy IssueChannelToken)

revokeChannelToken :: ChannelToken -> ClientM NoContent
revokeChannelToken = client (Proxy :: Proxy RevokeChannelToken)
