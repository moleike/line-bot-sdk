{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Line.Bot.Endpoints
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Endpoints where

import           Data.ByteString.Lazy (ByteString)
import           Line.Bot.Types
import           Servant.API
import           Servant.Client

-- | Combinator for authenticating with the channel access token
type ChannelAuth = AuthProtect "channel-access-token"

type GetProfile' a = ChannelAuth
  :> "v2" :> "bot"
  :> "profile"
  :> Capture "userId" (Id User)
  :> Get '[JSON] a

type GetProfile = GetProfile' Profile

type GetGroupMemberProfile' a = ChannelAuth
  :> "v2" :> "bot"
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] a

type GetGroupMemberProfile = GetGroupMemberProfile' Profile

type LeaveGroup = ChannelAuth
  :> "v2" :> "bot"
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type GetRoomMemberProfile' a = ChannelAuth
  :> "v2" :> "bot"
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] a

type GetRoomMemberProfile = GetRoomMemberProfile' Profile

type LeaveRoom = ChannelAuth
  :> "v2" :> "bot"
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type ReplyMessage' a = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "reply"
  :> ReqBody '[JSON] a
  :> PostNoContent '[JSON] NoContent

type ReplyMessage = ReplyMessage' ReplyMessageBody

type PushMessage' a = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "push"
  :> ReqBody '[JSON] a
  :> PostNoContent '[JSON] NoContent

type PushMessage = PushMessage' PushMessageBody

type MulticastMessage' a = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "multicast"
  :> ReqBody '[JSON] a
  :> PostNoContent '[JSON] NoContent

type MulticastMessage = MulticastMessage' MulticastMessageBody

type GetContent = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> Get '[OctetStream] ByteString

type GetReplyMessageCount' a b = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "delivery"
  :> "reply"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetReplyMessageCount = GetReplyMessageCount' LineDate MessageCount

type GetPushMessageCount' a b = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "delivery"
  :> "push"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetPushMessageCount = GetPushMessageCount' LineDate MessageCount

type GetMulticastMessageCount' a b = ChannelAuth
  :> "v2" :> "bot"
  :> "message"
  :> "delivery"
  :> "multicast"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetMulticastMessageCount = GetMulticastMessageCount' LineDate MessageCount

type IssueLinkToken' a = ChannelAuth
  :> "v2" :> "bot"
  :> "user"
  :> Capture "userId" (Id User)
  :> "linkToken"
  :> Get '[JSON] a

type IssueLinkToken = IssueLinkToken' LinkToken

type IssueChannelToken' a b =
  ReqBody '[FormUrlEncoded] a
  :> "v2" :> "bot"
  :> "oauth"
  :> "accessToken"
  :> Post '[JSON] b

type IssueChannelToken = IssueChannelToken' ClientCredentials ShortLivedChannelToken

type RevokeChannelToken' a =
  ReqBody '[FormUrlEncoded] a
  :> "v2" :> "bot"
  :> "oauth"
  :> "revoke"
  :> PostNoContent '[JSON] NoContent

type RevokeChannelToken = RevokeChannelToken' ChannelToken
