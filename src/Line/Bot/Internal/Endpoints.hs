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
-- Copyright   : (c) Alexandre Moreno, 2019-2021
-- License     : BSD-3-Clause
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Internal.Endpoints where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import           Line.Bot.Types
import           Servant.API

type GetProfile' a =
     "v2":> "bot" :> "profile"
  :> Capture "userId" (Id 'User)
  :> Get '[JSON] a

type GetProfile = GetProfile' Profile

type GetGroupMemberProfile' a =
     "v2":> "bot" :> "group"
  :> Capture "groupId" (Id 'Group)
  :> "member"
  :> Capture "userId" (Id 'User)
  :> Get '[JSON] a

type GetGroupMemberProfile = GetGroupMemberProfile' Profile

type LeaveGroup =
     "v2":> "bot" :> "group"
  :> Capture "groupId" (Id 'Group)
  :> "leave"
  :> PostNoContent

type GetGroupMemberUserIds' a =
     "v2":> "bot" :> "group"
  :> Capture "groupId" (Id 'Group)
  :> "members"
  :> "ids"
  :> QueryParam "start" String
  :> Get '[JSON] a

type GetGroupMemberUserIds = GetGroupMemberUserIds' MemberIds

type GetRoomMemberProfile' a =
     "v2":> "bot" :> "room"
  :> Capture "roomId" (Id 'Room)
  :> "member"
  :> Capture "userId" (Id 'User)
  :> Get '[JSON] a

type GetRoomMemberProfile = GetRoomMemberProfile' Profile

type LeaveRoom =
     "v2":> "bot" :> "room"
  :> Capture "roomId" (Id 'Room)
  :> "leave"
  :> PostNoContent

type GetRoomMemberUserIds' a =
     "v2":> "bot" :> "room"
  :> Capture "roomId" (Id 'Room)
  :> "members"
  :> "ids"
  :> QueryParam "start" String
  :> Get '[JSON] a

type GetRoomMemberUserIds = GetRoomMemberUserIds' MemberIds

type ReplyMessage' a =
     "v2":> "bot" :> "message"
  :> "reply"
  :> ReqBody '[JSON] a
  :> PostNoContent

type ReplyMessage = ReplyMessage' ReplyMessageBody

type PushMessage' a =
     "v2":> "bot" :> "message"
  :> "push"
  :> ReqBody '[JSON] a
  :> PostNoContent

type PushMessage = PushMessage' PushMessageBody

type MulticastMessage' a =
     "v2":> "bot" :> "message"
  :> "multicast"
  :> ReqBody '[JSON] a
  :> PostNoContent

type MulticastMessage = MulticastMessage' MulticastMessageBody

type BroadcastMessage' a =
     "v2":> "bot" :> "message"
  :> "broadcast"
  :> ReqBody '[JSON] a
  :> PostNoContent

type BroadcastMessage = BroadcastMessage' BroadcastMessageBody

type GetContent =
     "v2":> "bot" :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> Get '[OctetStream] LB.ByteString

type GetContentStream =
     "v2":> "bot" :> "message"
  :> Capture "messageId" MessageId
  :> "content"
  :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type GetReplyMessageCount' a b =
     "v2":> "bot" :> "message" :> "delivery"
  :> "reply"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetReplyMessageCount = GetReplyMessageCount' LineDate MessageCount

type GetPushMessageCount' a b =
     "v2":> "bot" :> "message" :> "delivery"
  :> "push"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetPushMessageCount = GetPushMessageCount' LineDate MessageCount

type GetMulticastMessageCount' a b =
     "v2" :> "bot" :> "message" :> "delivery"
  :> "multicast"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetMulticastMessageCount = GetMulticastMessageCount' LineDate MessageCount

type GetBroadcastMessageCount' a b =
     "v2" :> "bot" :> "message" :> "delivery"
  :> "broadcast"
  :> QueryParam' '[Required, Strict] "date" a
  :> Get '[JSON] b

type GetBroadcastMessageCount = GetBroadcastMessageCount' LineDate MessageCount

type GetMessageQuota' a =
     "v2":> "bot" :> "message" :> "quota"
  :> "consumption"
  :> Get '[JSON] a

type GetMessageQuota = GetMessageQuota' MessageQuota

type IssueLinkToken' a =
     "v2":> "bot" :> "user"
  :> Capture "userId" (Id 'User)
  :> "linkToken"
  :> Get '[JSON] a

type IssueLinkToken = IssueLinkToken' LinkToken

type IssueChannelToken' a b =
     "v2" :> "oauth"
  :> "accessToken"
  :> ReqBody '[FormUrlEncoded] a
  :> Post '[JSON] b

type IssueChannelToken = IssueChannelToken' ClientCredentials ShortLivedChannelToken

type RevokeChannelToken' a =
     "v2" :> "oauth"
  :> "revoke"
  :> ReqBody '[FormUrlEncoded] a
  :> Post '[JSON] NoContent

type RevokeChannelToken = RevokeChannelToken' ChannelToken

type CreateRichMenu' a b =
     "v2" :> "bot" :> "richmenu"
  :> ReqBody '[JSON] a
  :> Post '[JSON] b

type CreateRichMenu = CreateRichMenu' RichMenu RichMenuId

type DeleteRichMenu' a =
     "v2" :> "bot" :> "richmenu"
  :> Capture "richMenuId" a
  :> Delete '[JSON] NoContent

type DeleteRichMenu = DeleteRichMenu' RichMenuId

type GetRichMenu' a b =
     "v2" :> "bot" :> "richmenu"
  :> Capture "richMenuId" a
  :> Get '[JSON] b

type GetRichMenu = GetRichMenu' RichMenuId RichMenuResponse

type UploadRichMenuImageJpg' a b =
     "v2" :> "bot" :> "richmenu"
  :> Capture "richMenuId" a
  :> "content"
  :> ReqBody '[JPEG] b
  :> Post '[JSON] NoContent

type UploadRichMenuImageJpg = UploadRichMenuImageJpg' RichMenuId ByteString

type GetRichMenuList' a =
     "v2" :> "bot" :> "richmenu"
  :> "list"
  :> Get '[JSON] a

type GetRichMenuList = GetRichMenuList' RichMenuResponseList

type SetDefaultRichMenu' a =
     "v2":> "bot" :> "user" :> "all" :> "richmenu"
  :> Capture "richMenuId" a
  :> PostNoContent

type SetDefaultRichMenu = SetDefaultRichMenu' RichMenuId
