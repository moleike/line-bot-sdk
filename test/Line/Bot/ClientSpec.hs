{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Line.Bot.ClientSpec (spec) where

import           Control.Arrow                    (left)
import           Control.DeepSeq                  (NFData)
import           Control.Monad                    ((>=>))
import           Control.Monad.Free
import           Control.Monad.Trans.Reader       (runReaderT)
import           Data.Aeson                       (Value)
import           Data.Aeson.QQ
import           Data.ByteString                  as B (stripPrefix)
import           Data.Foldable                    (toList)
import           Data.Text
import           Data.Text.Encoding
import           Data.Time.Calendar               (fromGregorian)
import           Line.Bot.Client                  hiding (runLine)
import           Line.Bot.Internal.Auth
import           Line.Bot.Internal.Endpoints
import           Line.Bot.Types
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Types               (hAuthorization)
import           Network.Wai                      as Wai (Request,
                                                          requestHeaders)
import           Network.Wai.Handler.Warp         (Port, withApplication)
import           Servant
import           Servant.Client.Core
import           Servant.Client.Free              as F
import           Servant.Client.Streaming
import           Servant.Server                   (Context (..))
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.Hspec.Wai

type instance AuthServerData ChannelAuth = ChannelToken

-- a dummy auth handler that returns the channel access token
authHandler :: AuthHandler Wai.Request ChannelToken
authHandler = mkAuthHandler $ \request ->
  case lookup hAuthorization (Wai.requestHeaders request) >>= B.stripPrefix "Bearer " of
    Nothing -> throwError $ err401 { errBody = "Bad" }
    Just t  -> return $ ChannelToken $ decodeUtf8 t

serverContext :: Context '[AuthHandler Wai.Request ChannelToken]
serverContext = authHandler :. EmptyContext

type API = GetProfile' Value
      :<|> GetGroupMemberProfile' Value
      :<|> GetRoomMemberProfile' Value

getReplyMessageCountF :: LineDate -> Auth -> Free ClientF MessageCount
getReplyMessageCountF = F.client (Proxy :: Proxy GetReplyMessageCount)

getPushMessageCountF :: LineDate -> Auth -> Free ClientF MessageCount
getPushMessageCountF = F.client (Proxy :: Proxy GetPushMessageCount)

getMulticastMessageCountF :: LineDate -> Auth -> Free ClientF MessageCount
getMulticastMessageCountF = F.client (Proxy :: Proxy GetMulticastMessageCount)

testProfile :: Value
testProfile = [aesonQQ|
  {
      displayName: "LINE taro",
      userId: "U4af4980629...",
      pictureUrl: "https://obs.line-apps.com/...",
      statusMessage: "Hello, LINE!"
  }
|]

withPort :: Port -> (ClientEnv -> IO a) -> IO a
withPort port app = do
  manager <- newManager defaultManagerSettings
  app $ mkClientEnv manager $ BaseUrl Http "localhost" port ""

token :: ChannelToken
token = "fake"

runLine :: NFData a => Line a -> Port -> IO (Either ClientError a)
runLine comp port = withPort port $ runClientM $ runReaderT comp token

app :: Application
app = serveWithContext (Proxy :: Proxy API) serverContext $
       (\_ _ -> return testProfile)
  :<|> (\_ _ _ -> return testProfile)
  :<|> (\_ _ _ -> return testProfile)

spec :: Spec
spec = describe "Line client" $ do
  it "should return user profile" $
    withApplication (pure app) $
      runLine (getProfile "1") >=> (`shouldSatisfy` isRight)

  it "should return group user profile" $
    withApplication (pure app) $
      runLine (getGroupMemberProfile "1" "1") >=> (`shouldSatisfy` isRight)

  it "should return room user profile" $
    withApplication (pure app) $
      runLine (getRoomMemberProfile "1" "1") >=> (`shouldSatisfy` isRight)

  it "should send `date` query param for push message count" $ do
    let Free (RunRequest Request{..} _) = getPushMessageCountF date (mkAuth token)
    toList requestQueryString `shouldBe` [("date", Just "20190407")]

  it "should send `date` query param for reply message count" $ do
    let Free (RunRequest Request{..} _) = getReplyMessageCountF date (mkAuth token)
    toList requestQueryString `shouldBe` [("date", Just "20190407")]

  it "should send `date` query param for multicast message count" $ do
    let Free (RunRequest Request{..} _) = getMulticastMessageCountF date (mkAuth token)
    toList requestQueryString `shouldBe` [("date", Just "20190407")]
  where
    date = LineDate $ fromGregorian 2019 4 7

