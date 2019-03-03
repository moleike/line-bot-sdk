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

type GetProfile = ChannelAuth
  :> "profile"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type GetGroupMemberProfile = ChannelAuth
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type LeaveGroup = ChannelAuth
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type GetRoomMemberProfile = ChannelAuth
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type LeaveRoom = ChannelAuth
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type ReplyMessage = ChannelAuth
  :> ReqBody '[JSON] ReplyMessageBody
  :> "message"
  :> "reply"
  :> PostNoContent '[JSON] NoContent

type PushMessage = ChannelAuth
  :> ReqBody '[JSON] PushMessageBody
  :> "message"
  :> "push"
  :> PostNoContent '[JSON] NoContent

type MulticastMessage = ChannelAuth
  :> ReqBody '[JSON] MulticastMessageBody
  :> "message"
  :> "multicast"
  :> PostNoContent '[JSON] NoContent

type GetContent = ChannelAuth
  :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> Get '[OctetStream] ByteString

type IssueLinkToken = ChannelAuth
  :> "user"
  :> Capture "userId" (Id User)
  :> "linkToken"
  :> Get '[JSON] LinkToken

type IssueChannelToken =
  ReqBody '[FormUrlEncoded] ClientCredentials
  :> "oauth"
  :> "accessToken"
  :> Post '[JSON] ShortLivedChannelToken

type RevokeChannelToken =
  ReqBody '[FormUrlEncoded] ChannelToken
  :> "oauth"
  :> "revoke"
  :> PostNoContent '[JSON] NoContent

type Endpoints = "v2" :> "bot" :>
  (    GetProfile
  :<|> GetGroupMemberProfile
  :<|> LeaveGroup
  :<|> GetRoomMemberProfile
  :<|> LeaveRoom
  :<|> ReplyMessage
  :<|> PushMessage
  :<|> MulticastMessage
  :<|> GetContent
  :<|> IssueLinkToken
  :<|> IssueChannelToken
  :<|> RevokeChannelToken
  )
