{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Line.Bot.Internal.Endpoints
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Internal.Endpoints where

import           Data.ByteString.Lazy (ByteString)
import           Line.Bot.Types
import           Servant.API
import           Servant.Client

-- | Combinator for authenticating with the channel access token
type ChannelAuth = AuthProtect "channel-access-token"

type GetProfile' a =
     "v2":> "bot"
  :> "profile"
  :> Capture "userId" (Id User)
  :> ChannelAuth
  :> Get '[JSON] a

type GetProfile = GetProfile' Profile

type GetGroupMemberProfile' a =
     "v2":> "bot"
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "member"
  :> Capture "userId" (Id User)
  :> ChannelAuth
  :> Get '[JSON] a

type GetGroupMemberProfile = GetGroupMemberProfile' Profile

type LeaveGroup =
     "v2":> "bot"
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "leave"
  :> ChannelAuth
  :> PostNoContent '[JSON] NoContent

type GetRoomMemberProfile' a =
     "v2":> "bot"
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "member"
  :> Capture "userId" (Id User)
  :> ChannelAuth
  :> Get '[JSON] a

type GetRoomMemberProfile = GetRoomMemberProfile' Profile

type LeaveRoom =
     "v2":> "bot"
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "leave"
  :> ChannelAuth
  :> PostNoContent '[JSON] NoContent

type ReplyMessage' a =
     "v2":> "bot"
  :> "message"
  :> "reply"
  :> ReqBody '[JSON] a
  :> ChannelAuth
  :> PostNoContent '[JSON] NoContent

type ReplyMessage = ReplyMessage' ReplyMessageBody

type PushMessage' a =
     "v2":> "bot"
  :> "message"
  :> "push"
  :> ReqBody '[JSON] a
  :> ChannelAuth
  :> PostNoContent '[JSON] NoContent

type PushMessage = PushMessage' PushMessageBody

type MulticastMessage' a =
     "v2":> "bot"
  :> "message"
  :> "multicast"
  :> ReqBody '[JSON] a
  :> ChannelAuth
  :> PostNoContent '[JSON] NoContent

type MulticastMessage = MulticastMessage' MulticastMessageBody

type GetContent =
     "v2":> "bot"
  :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> ChannelAuth
  :> Get '[OctetStream] ByteString

type GetContentStream =
     "v2":> "bot"
  :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> ChannelAuth
  :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type GetReplyMessageCount' a b =
     "v2":> "bot"
  :> "message"
  :> "delivery"
  :> "reply"
  :> QueryParam' '[Required, Strict] "date" a
  :> ChannelAuth
  :> Get '[JSON] b

type GetReplyMessageCount = GetReplyMessageCount' LineDate MessageCount

type GetPushMessageCount' a b =
     "v2":> "bot"
  :> "message"
  :> "delivery"
  :> "push"
  :> QueryParam' '[Required, Strict] "date" a
  :> ChannelAuth
  :> Get '[JSON] b

type GetPushMessageCount = GetPushMessageCount' LineDate MessageCount

type GetMulticastMessageCount' a b =
     "v2" :> "bot"
  :> "message"
  :> "delivery"
  :> "multicast"
  :> QueryParam' '[Required, Strict] "date" a
  :> ChannelAuth
  :> Get '[JSON] b

type GetMulticastMessageCount = GetMulticastMessageCount' LineDate MessageCount

type GetMessageQuota' a =
     "v2":> "bot"
  :> "message"
  :> "quota"
  :> "consumption"
  :> ChannelAuth
  :> Get '[JSON] a

type GetMessageQuota = GetMessageQuota' MessageQuota

type IssueLinkToken' a =
     "v2":> "bot"
  :> "user"
  :> Capture "userId" (Id User)
  :> "linkToken"
  :> ChannelAuth
  :> Get '[JSON] a

type IssueLinkToken = IssueLinkToken' LinkToken

type IssueChannelToken' a b =
  ReqBody '[FormUrlEncoded] a
  :> "v2"
  :> "oauth"
  :> "accessToken"
  :> Post '[JSON] b

type IssueChannelToken = IssueChannelToken' ClientCredentials ShortLivedChannelToken

type RevokeChannelToken' a =
  ReqBody '[FormUrlEncoded] a
  :> "v2"
  :> "oauth"
  :> "revoke"
  :> PostNoContent '[JSON] NoContent

type RevokeChannelToken = RevokeChannelToken' ChannelToken
