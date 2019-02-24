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

data ChannelAuth

type GetProfile = AuthProtect ChannelAuth
  :> "profile"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type GetGroupMemberProfile = AuthProtect ChannelAuth
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type LeaveGroup = AuthProtect ChannelAuth
  :> "group"
  :> Capture "groupId" (Id Group)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type GetRoomMemberProfile = AuthProtect ChannelAuth
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "member"
  :> Capture "userId" (Id User)
  :> Get '[JSON] Profile

type LeaveRoom = AuthProtect ChannelAuth
  :> "room"
  :> Capture "roomId" (Id Room)
  :> "leave"
  :> PostNoContent '[JSON] NoContent

type ReplyMessage = AuthProtect ChannelAuth
  :> ReqBody '[JSON] ReplyMessageBody
  :> "message"
  :> "reply"
  :> PostNoContent '[JSON] NoContent

type PushMessage = AuthProtect ChannelAuth
  :> ReqBody '[JSON] PushMessageBody
  :> "message"
  :> "push"
  :> PostNoContent '[JSON] NoContent

type MulticastMessage = AuthProtect ChannelAuth
  :> ReqBody '[JSON] MulticastMessageBody
  :> "message"
  :> "multicast"
  :> PostNoContent '[JSON] NoContent

type GetContent = AuthProtect ChannelAuth
  :> "message"
  :> Capture "messageId" String
  :> "content"
  :> Get '[OctetStream] ByteString

type IssueLinkToken = AuthProtect ChannelAuth
  :> "user"
  :> Capture "userId" (Id User)
  :> "linkToken"
  :> Get '[JSON] LinkToken

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
  )
