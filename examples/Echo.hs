{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad                        (mapM_)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import           Data.String                          (fromString)
import           Line.Bot.Client                      as B
import           Line.Bot.Webhook                     as W
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant
import           Servant.Server                       (Context ((:.), EmptyContext))
import           System.Environment                   (getEnv)

type WebM = ReaderT ChannelToken Handler

type API = "webhook" :> Webhook

echo :: Event -> Line NoContent
echo Message { message = W.Text { text }, replyToken } =
  replyMessage replyToken [B.Text text]
echo _ = return NoContent

handleEvents :: Events -> WebM NoContent
handleEvents Events { events } = do
  token <- ask
  _     <- liftIO $ mapM_ (runLine token . echo) events
  return NoContent

echoServer :: ServerT API WebM
echoServer = handleEvents

app :: ChannelToken -> ChannelSecret -> Application
app token secret = serveWithContext api context server
  where
    api     = Proxy :: Proxy API
    server  = hoistServerWithContext api pc nt echoServer
    nt      = flip runReaderT token
    pc      = Proxy :: Proxy '[ChannelSecret]
    context = secret :. EmptyContext

main :: IO ()
main = do
  token  <- fromString <$> getEnv "CHANNEL_TOKEN"
  secret <- fromString <$> getEnv "CHANNEL_SECRET"
  port   <- read       <$> getEnv "PORT"
  run port $ logStdout $ app token secret
