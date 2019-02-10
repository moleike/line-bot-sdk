{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Line.Bot.Client
  ( runLine
  , Line
  , ChannelToken(..)
  , getProfile
  , getGroupMemberProfile
  , leaveGroup
  , getRoomMemberProfile
  , leaveRoom
  , replyMessage
  , pushMessage
  , multicastMessage
  )
where

import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Monoid                          ((<>))
import           Data.Proxy
import           Data.String
import           Data.Text                            as T
import           Line.Bot.Data
import           Line.Bot.Endpoints
import           Network.HTTP.Client                  (newManager)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           Servant.API                          hiding (addHeader)
import           Servant.Client
import           Servant.Client.Core.Internal.Auth    (AuthClientData,
                                                       AuthenticatedRequest,
                                                       mkAuthenticatedRequest)
import           Servant.Client.Core.Internal.Request (Request, addHeader)
import           Servant.Server.Experimental.Auth     (AuthHandler,
                                                       AuthServerData,
                                                       mkAuthHandler)


host :: BaseUrl
host = BaseUrl Https "api.line.me" 443 ""

newtype ChannelToken = ChannelToken Text
  deriving (Eq)

instance IsString ChannelToken where
  fromString s = ChannelToken (fromString s)

instance ToHttpApiData ChannelToken where
  toQueryParam (ChannelToken t) = "Bearer " <> t

type Line = ReaderT ChannelToken ClientM

type Auth = AuthenticatedRequest (AuthProtect ChannelAuth)

type instance AuthClientData (AuthProtect ChannelAuth) = ChannelToken

runLine' :: ClientM a -> IO (Either ServantError a)
runLine' comp = do
  manager <- newManager tlsManagerSettings
  runClientM comp (mkClientEnv manager host)

runLine :: ChannelToken -> Line a -> IO (Either ServantError a)
runLine token comp = runLine' $ runReaderT comp token

mkAuth :: ChannelToken -> Auth
mkAuth token = mkAuthenticatedRequest token addAuthHeader
 where
  addAuthHeader :: ChannelToken -> Request -> Request
  addAuthHeader = addHeader "Authorization"

getProfile' :: Auth -> Id User -> ClientM Profile

getGroupMemberProfile' :: Auth -> Id Group -> Id User -> ClientM Profile

leaveGroup' :: Auth -> Id Group -> ClientM NoContent

getRoomMemberProfile' :: Auth -> Id Room -> Id User -> ClientM Profile

leaveRoom' :: Auth -> Id Room -> ClientM NoContent

replyMessage' :: Auth -> ReplyMessageBody -> ClientM NoContent

pushMessage' :: Auth -> PushMessageBody -> ClientM NoContent

multicastMessage' :: Auth -> MulticastMessageBody -> ClientM NoContent

getContent' :: Auth -> String -> ClientM ByteString

getProfile'
  :<|> getGroupMemberProfile'
  :<|> leaveGroup'
  :<|> getRoomMemberProfile'
  :<|> leaveRoom'
  :<|> replyMessage'
  :<|> pushMessage'
  :<|> multicastMessage'
  :<|> getContent' = client (Proxy :: Proxy Endpoints)

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
