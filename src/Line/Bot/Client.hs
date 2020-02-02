{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
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
  , withLine
  , withLineEnv
  -- ** Profile
  , getProfile
  -- ** Group
  , getGroupMemberProfile
  , leaveGroup
  , getGroupMemberUserIds
  -- ** Room
  , getRoomMemberProfile
  , leaveRoom
  , getRoomMemberUserIds
  -- ** Message
  , replyMessage
  , pushMessage
  , multicastMessage
  , broadcastMessage
  , getContent
  , getContentS
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
  -- ** Rich menus
  , createRichMenu
  , deleteRichMenu
  , getRichMenu
  , uploadRichMenuImageJpg
  , getRichMenuList
  , setDefaultRichMenu
  )
where

import           Control.DeepSeq             (NFData)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class   (lift)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LB
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Time.Calendar          (Day)
import           Line.Bot.Internal.Auth      (Auth, mkAuth)
import           Line.Bot.Internal.Endpoints
import           Line.Bot.Types
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Servant.API
import           Servant.Client.Streaming

defaultEndpoint :: BaseUrl
defaultEndpoint = BaseUrl Https "api.line.me" 443 ""

blobEndpoint :: BaseUrl
blobEndpoint = BaseUrl Https "api-data.line.me" 443 ""

-- | @Line@ is the monad in which bot requests run. Contains the
-- OAuth access token for a channel
type Line = ReaderT ChannelToken ClientM

-- | Perform a request using LINE 'ClientEnv'
--
-- > withLineEnv $ \env -> runClientM comp env
withLineEnv :: (ClientEnv -> IO a) -> IO a
withLineEnv app = do
  manager <- newManager tlsManagerSettings
  app $ mkClientEnv manager defaultEndpoint

runLine' :: NFData a => ClientM a -> IO (Either ClientError a)
runLine' comp = withLineEnv $ \env -> runClientM comp env

-- | Executes a request in the LINE plaform with the given 'ChannelToken'
runLine :: NFData a => Line a -> ChannelToken -> IO (Either ClientError a)
runLine comp = runLine' . runReaderT comp

withLine' :: ClientM a -> (Either ClientError a -> IO b) -> IO b
withLine' comp k = withLineEnv $ \env -> withClientM comp env k

-- | Execute a request with a streaming response in the LINE platform
withLine :: Line a -> ChannelToken -> (Either ClientError a -> IO b) -> IO b
withLine comp = withLine' . runReaderT comp

withHost :: MonadReader ClientEnv m => BaseUrl -> m a -> m a
withHost baseUrl = local (\env -> env { baseUrl = baseUrl })

type LineAuth a = Auth -> ClientM a

type family AddLineAuth a :: * where
  AddLineAuth (LineAuth a) = Line a
  AddLineAuth (a -> b) = a -> AddLineAuth b

class HasLine a where
  addLineAuth :: a -> AddLineAuth a

instance HasLine (LineAuth a) where
  addLineAuth comp = ask >>= lift . comp . mkAuth

instance HasLine (a -> LineAuth b) where
  addLineAuth comp = addLineAuth . comp

instance HasLine (a -> b -> LineAuth c) where
  addLineAuth comp = addLineAuth . comp

line :: (HasLine (Client ClientM api), HasClient ClientM api)
     => Proxy api
     -> AddLineAuth (Client ClientM api)
line = addLineAuth . client

unfoldMemberUserIds :: (Maybe String -> Line MemberIds) -> Line [Id User]
unfoldMemberUserIds k = go Nothing where
  go tok = do
    MemberIds{next, memberIds = a} <- k tok
    as <- maybe (return []) (\_ -> go next) next
    return $ a ++ as

getProfile :: Id User -> Line Profile
getProfile = line (Proxy @GetProfile)

getGroupMemberProfile :: Id Group -> Id User -> Line Profile
getGroupMemberProfile = line (Proxy @GetGroupMemberProfile)

leaveGroup :: Id Group -> Line NoContent
leaveGroup = line (Proxy @LeaveGroup)

getGroupMemberUserIds' :: Id Group -> Maybe String -> Line MemberIds
getGroupMemberUserIds' = line (Proxy @GetGroupMemberUserIds)

getGroupMemberUserIds :: Id Group -> Line [Id User]
getGroupMemberUserIds = unfoldMemberUserIds . getGroupMemberUserIds'

getRoomMemberProfile :: Id Room -> Id User -> Line Profile
getRoomMemberProfile = line (Proxy @GetRoomMemberProfile)

leaveRoom :: Id Room -> Line NoContent
leaveRoom = line (Proxy @LeaveRoom)

getRoomMemberUserIds' :: Id Room -> Maybe String -> Line MemberIds
getRoomMemberUserIds' = line (Proxy @GetRoomMemberUserIds)

getRoomMemberUserIds :: Id Room -> Line [Id User]
getRoomMemberUserIds = unfoldMemberUserIds . getRoomMemberUserIds'

replyMessage' :: ReplyMessageBody -> Line NoContent
replyMessage' = line (Proxy @ReplyMessage)

replyMessage :: ReplyToken -> [Message] -> Line NoContent
replyMessage a ms = replyMessage' (ReplyMessageBody a ms)

pushMessage' :: PushMessageBody -> Line NoContent
pushMessage' = line (Proxy @PushMessage)

pushMessage :: Id a -> [Message] -> Line NoContent
pushMessage a ms = pushMessage' (PushMessageBody a ms)

multicastMessage' :: MulticastMessageBody -> Line NoContent
multicastMessage' = line (Proxy @MulticastMessage)

multicastMessage :: [Id User] -> [Message] -> Line NoContent
multicastMessage a ms = multicastMessage' (MulticastMessageBody a ms)

broadcastMessage' :: BroadcastMessageBody -> Line NoContent
broadcastMessage' = line (Proxy @BroadcastMessage)

broadcastMessage :: [Message] -> Line NoContent
broadcastMessage = broadcastMessage' . BroadcastMessageBody

getContent' :: MessageId -> Auth -> ClientM LB.ByteString
getContent' = client (Proxy @GetContent)

getContent :: MessageId -> Line LB.ByteString
getContent  a = ask >>= lift . withHost blobEndpoint . getContent' a . mkAuth

getContentS' :: MessageId -> Auth -> ClientM (SourceIO ByteString)
getContentS' = client (Proxy @GetContentStream)

-- | This is an streaming version of 'getContent' meant to be used with coroutine
-- libraries like @pipes@, @conduits@, @streaming@, etc. You need and instance
-- of 'FromSourceIO', see e.g. @servant-conduit@.
--
-- Example:
--
-- > getContentC :: MessageId -> Line (ConduitT () ByteString IO ())
-- > getContentC = fmap fromSourceIO . getContentS
getContentS :: MessageId -> Line (SourceIO ByteString)
getContentS  a = ask >>= lift . withHost blobEndpoint . getContentS' a . mkAuth

getPushMessageCount' :: LineDate -> Line MessageCount
getPushMessageCount' = line (Proxy @GetPushMessageCount)

getPushMessageCount :: Day -> Line (Maybe Int)
getPushMessageCount = fmap count . getPushMessageCount' . LineDate

getReplyMessageCount' :: LineDate -> Line MessageCount
getReplyMessageCount' = line (Proxy @GetReplyMessageCount)

getReplyMessageCount :: Day -> Line (Maybe Int)
getReplyMessageCount = fmap count . getReplyMessageCount' . LineDate

getMulticastMessageCount' :: LineDate -> Line MessageCount
getMulticastMessageCount' = line (Proxy @GetMulticastMessageCount)

getMulticastMessageCount :: Day -> Line (Maybe Int)
getMulticastMessageCount = fmap count . getMulticastMessageCount' . LineDate

getBroadcastMessageCount' :: LineDate -> Line MessageCount
getBroadcastMessageCount' = line (Proxy @GetBroadcastMessageCount)

getBroadcastMessageCount :: Day -> Line (Maybe Int)
getBroadcastMessageCount = fmap count . getBroadcastMessageCount' . LineDate

getMessageQuota' :: Line MessageQuota
getMessageQuota' = line (Proxy @GetMessageQuota)

getMessageQuota :: Line Int
getMessageQuota = fmap totalUsage getMessageQuota'

issueLinkToken :: Id User -> Line LinkToken
issueLinkToken = line (Proxy @IssueLinkToken)

issueChannelToken' :: ClientCredentials -> ClientM ShortLivedChannelToken
issueChannelToken' = client (Proxy @IssueChannelToken)

issueChannelToken :: ChannelId -> ChannelSecret -> ClientM ShortLivedChannelToken
issueChannelToken a b = issueChannelToken' $ ClientCredentials a b

revokeChannelToken :: ChannelToken -> ClientM NoContent
revokeChannelToken = client (Proxy @RevokeChannelToken)

createRichMenu :: RichMenu -> Line RichMenuId
createRichMenu = line (Proxy @CreateRichMenu)

getRichMenu' :: RichMenuId -> Line RichMenuResponse
getRichMenu' = line (Proxy @GetRichMenu)

getRichMenu :: RichMenuId -> Line RichMenu
getRichMenu = fmap richMenu . getRichMenu'

uploadRichMenuImageJpg' :: RichMenuId -> ByteString -> Auth -> ClientM NoContent
uploadRichMenuImageJpg' = client (Proxy @UploadRichMenuImageJpg)

uploadRichMenuImageJpg :: RichMenuId -> ByteString -> Line NoContent
uploadRichMenuImageJpg a b = ask >>= lift . withHost blobEndpoint . uploadRichMenuImageJpg' a b . mkAuth

deleteRichMenu :: RichMenuId -> Line NoContent
deleteRichMenu = line (Proxy @DeleteRichMenu)

getRichMenuList' :: Line RichMenuResponseList
getRichMenuList' =  line (Proxy @GetRichMenuList)

getRichMenuList :: Line [(RichMenuId, RichMenu)]
getRichMenuList = richmenus <$> getRichMenuList' <&> fmap f where
  f RichMenuResponse{..} = (RichMenuId richMenuId, richMenu)

setDefaultRichMenu :: RichMenuId -> Line NoContent
setDefaultRichMenu = line (Proxy @SetDefaultRichMenu)
