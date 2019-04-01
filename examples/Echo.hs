{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module Main (main) where

import           Control.Monad                        (forM_)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import           Data.String                          (fromString)
import           Line.Bot.Client                      (Line, replyMessage,
                                                       runLine)
import           Line.Bot.Types                       as B
import           Line.Bot.Webhook                     as W
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant
import           Servant.Server                       (Context ((:.), EmptyContext))
import           System.Environment                   (getEnv)

type WebM = ReaderT ChannelToken Handler

type API = "webhook" :> Webhook

echo :: Event -> Line NoContent
echo EventMessage { message = W.MessageText { text }, replyToken } =
  replyMessage replyToken [B.MessageText text Nothing]
echo _ = return NoContent

handleEvents :: [Event] -> WebM NoContent
handleEvents events = do
  token <- ask
  _     <- liftIO $ forM_ events $ flip runLine token . echo
  return NoContent

echoServer :: ServerT API WebM
echoServer = handleEvents . events

app :: ChannelToken -> ChannelSecret -> Application
app token secret = serveWithContext api context server
  where
    api = Proxy :: Proxy API
    pc = Proxy :: Proxy '[ChannelSecret]
    server = hoistServerWithContext api pc (`runReaderT` token) echoServer
    context = secret :. EmptyContext

main :: IO ()
main = do
  token  <- fromString <$> getEnv "CHANNEL_TOKEN"
  secret <- fromString <$> getEnv "CHANNEL_SECRET"
  port   <- read       <$> getEnv "PORT"
  run port $ logStdout $ app token secret
