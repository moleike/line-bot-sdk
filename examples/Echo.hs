{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad              (mapM_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.String                (fromString)
import           Line.Bot                   as B
import           Line.Bot.Client
import           Line.Bot.Webhook           as W
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           System.Environment         (getEnv)


type WebM = ReaderT ChannelToken Handler

type Webhook = "webhook" :> ReqBody '[JSON] Events
  :> PostNoContent '[JSON] NoContent

echo :: Event -> Line NoContent
echo Message { message = W.Text { text }, replyToken } =
  replyMessage replyToken [B.Text text]
echo _ = return NoContent

handleEvents :: Events -> WebM NoContent
handleEvents Events { events } = do
  token <- ask
  _     <- liftIO $ mapM_ (\ev -> runLine token $ echo ev) events
  return NoContent

echoServer :: ServerT Webhook WebM
echoServer = handleEvents

webhook :: Proxy Webhook
webhook = Proxy

app :: ChannelToken -> Application
app token = serve webhook server
  where
    server = hoistServer webhook nt echoServer
    nt     = flip runReaderT token

main :: IO ()
main = do
  token <- fromString <$> getEnv "CHANNEL_TOKEN"
  run 8080 $ app token
