{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

module Line.Bot.WebhookSpec (spec) where

import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Aeson                (Value, encode)
import           Data.Aeson.QQ
import           Data.Aeson.Types          (emptyObject)
import qualified Data.ByteString.Base64    as Base64
import           Line.Bot.Types            (ChannelSecret (..))
import           Line.Bot.Webhook          (Webhook)
import           Line.Bot.Webhook.Events   (Events)
import           Network.HTTP.Types        (HeaderName, hContentType)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Servant
import           Servant.Server            (Context ((:.), EmptyContext))
import           Test.Hspec                hiding (context)
import           Test.Hspec.Wai

hSignature :: HeaderName
hSignature = "X-Line-Signature"

secret :: ChannelSecret
secret = "shhhh"

context :: Context (ChannelSecret ': '[])
context = secret :. EmptyContext

app :: Application
app = serveWithContext (Proxy :: Proxy Webhook) context webhook

webhook :: Server Webhook
webhook = handleEvents

handleEvents :: Events -> Handler NoContent
handleEvents _ = return NoContent

testBody :: Value
testBody = [aesonQQ|
  {
    destination: "xxxxxxxxxx",
    events: [
      {
        replyToken: "8cf9239d56244f4197887e939187e19e",
        type: "follow",
        timestamp: 1462629479859,
        source: {
          type: "user",
          userId: "U4af4980629..."
        }
      }
    ]
  }
|]

spec :: Spec
spec = with (pure app) $ do
  describe "Webhook server" $ do

    it "should return 200 with a signed request" $ do
      let body    = encode testBody
          digest  = Base64.encode $ SHA256.hmaclazy (unChannelSecret secret) body
          headers = [(hContentType, "application/json"), (hSignature, digest)]

      request methodPost "/" headers body `shouldRespondWith` 200

    it "should return 400 for an invalid body" $ do
      let body    = encode emptyObject
          digest  = Base64.encode $ SHA256.hmaclazy (unChannelSecret secret) body
          headers = [(hContentType, "application/json"), (hSignature, digest)]

      request methodPost "/" headers body `shouldRespondWith` 400

    it "should return 401 for requests missing the signature header" $ do
      let body    = encode testBody
          headers = [(hContentType, "application/json")]

      request methodPost "/" headers body `shouldRespondWith` 401

    it "should return 401 when secret is incorrect" $ do
      let body    = encode testBody
          digest  = Base64.encode $ SHA256.hmaclazy "incorrect" body
          headers = [(hContentType, "application/json"), (hSignature, digest)]

      request methodPost "/" headers body `shouldRespondWith` 401
