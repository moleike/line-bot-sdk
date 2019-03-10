{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

module Line.Bot.WebhookSpec (spec) where

import qualified Control.Concurrent        as C
import           Control.Exception         (bracket)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (Value, encode)
import           Data.Aeson.QQ
import           Line.Bot.Types            (ChannelSecret)
import           Line.Bot.Webhook          (Webhook)
import           Line.Bot.Webhook.Events
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types        (hContentType)
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant
import           Servant.Server            (Context ((:.), EmptyContext))
import           Test.Hspec

secret :: Context (ChannelSecret ': '[])
secret = "shhhh" :. EmptyContext

app :: Application
app = serveWithContext (Proxy :: Proxy Webhook) secret webhook

webhook :: Server Webhook
webhook = handleEvents . events

handleEvents :: [Event] -> Handler NoContent
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

withApp :: IO () -> IO ()
withApp action =
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 app)
    C.killThread
    (const action)

spec :: Spec
spec = around_ withApp $ do
  describe "Webhook server" $ do

    manager <- runIO $ newManager defaultManagerSettings
    initialRequest <- runIO $ parseRequest "POST http://localhost:8888"

    it "should return 401 for requests missing the signature header" $ do
      let request = initialRequest {
          requestBody    = RequestBodyLBS $ encode testBody
        , requestHeaders =
            (hContentType, "application/json")
            : filter (\(x, _) -> x /= hContentType) (requestHeaders initialRequest)
        }

      response <- httpLbs request manager
      responseStatus response `shouldBe` status401
