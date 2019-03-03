{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
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
  -- ** Account Link
  , issueLinkToken
  -- ** OAuth
  , issueChannelToken
  , revokeChannelToken
  )
where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Proxy
import           Data.String
import           Data.Text                  as T
import           Line.Bot.Client.Auth
import           Line.Bot.Endpoints
import           Line.Bot.Types
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.API                hiding (addHeader)
import           Servant.Client

host :: BaseUrl
host = BaseUrl Https "api.line.me" 443 ""

-- | @Line@ is the monad in which bot requests run. Contains the
-- OAuth access token for a channel
type Line = ReaderT ChannelToken ClientM

-- | Executes a request in the LINE plaform (default)
withLineEnv :: (ClientEnv -> IO a) -> IO a
withLineEnv app = do
  manager <- newManager tlsManagerSettings
  app $ mkClientEnv manager host

runLine' :: ClientM a -> IO (Either ServantError a)
runLine' comp = withLineEnv $ \env -> runClientM comp env

-- | Runs a @Line@ computation with the given channel access token
runLine :: Line a -> ChannelToken -> IO (Either ServantError a)
runLine comp token = runLine' $ runReaderT comp token

getProfile' :: Auth -> Id User -> ClientM Profile

getGroupMemberProfile' :: Auth -> Id Group -> Id User -> ClientM Profile

leaveGroup' :: Auth -> Id Group -> ClientM NoContent

getRoomMemberProfile' :: Auth -> Id Room -> Id User -> ClientM Profile

leaveRoom' :: Auth -> Id Room -> ClientM NoContent

replyMessage' :: Auth -> ReplyMessageBody -> ClientM NoContent

pushMessage' :: Auth -> PushMessageBody -> ClientM NoContent

multicastMessage' :: Auth -> MulticastMessageBody -> ClientM NoContent

getContent' :: Auth -> MessageId -> ClientM ByteString

issueLinkToken' :: Auth -> Id User -> ClientM LinkToken

issueChannelToken :: ClientCredentials -> ClientM ShortLivedChannelToken

revokeChannelToken :: ChannelToken -> ClientM NoContent

getProfile'
  :<|> getGroupMemberProfile'
  :<|> leaveGroup'
  :<|> getRoomMemberProfile'
  :<|> leaveRoom'
  :<|> replyMessage'
  :<|> pushMessage'
  :<|> multicastMessage'
  :<|> getContent'
  :<|> issueLinkToken'
  :<|> issueChannelToken
  :<|> revokeChannelToken = client (Proxy :: Proxy Endpoints)

getProfile :: Id User -> Line Profile
getProfile a = ask >>= \token -> lift $ getProfile' (mkAuth token) a

getGroupMemberProfile :: Id Group -> Id User -> Line Profile
getGroupMemberProfile a b =
  ask >>= \token -> lift $ getGroupMemberProfile' (mkAuth token) a b

leaveGroup :: Id Group -> Line NoContent
leaveGroup a = ask >>= \token -> lift $ leaveGroup' (mkAuth token) a

getRoomMemberProfile :: Id Room -> Id User -> Line Profile
getRoomMemberProfile a b =
  ask >>= \token -> lift $ getRoomMemberProfile' (mkAuth token) a b

leaveRoom :: Id Room -> Line NoContent
leaveRoom a = ask >>= \token -> lift $ leaveRoom' (mkAuth token) a

replyMessage :: ReplyToken -> [Message] -> Line NoContent
replyMessage a ms = ask >>= \token -> lift $ replyMessage' (mkAuth token) body
  where body = ReplyMessageBody a ms

pushMessage :: Id a -> [Message] -> Line NoContent
pushMessage a ms = ask >>= \token -> lift $ pushMessage' (mkAuth token) body
  where body = PushMessageBody a ms

multicastMessage :: [Id User] -> [Message] -> Line NoContent
multicastMessage a ms = ask
  >>= \token -> lift $ multicastMessage' (mkAuth token) body
  where body = MulticastMessageBody a ms

getContent :: MessageId -> Line ByteString
getContent a = ask >>= \token -> lift $ getContent' (mkAuth token) a

issueLinkToken :: Id User -> Line LinkToken
issueLinkToken a = ask >>= \token -> lift $ issueLinkToken' (mkAuth token) a

