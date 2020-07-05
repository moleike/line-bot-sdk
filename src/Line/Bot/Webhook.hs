{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Line.Bot.Webhook
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Webhook
  ( Webhook
  , webhook
  , LineReqBody
  , module Events
  )
where

import           Control.Monad            (forM_)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Crypto.Hash.SHA256       as SHA256
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import           Data.Proxy
import           Data.String.Conversions  (cs)
import           Data.Typeable            (Typeable)
import           Line.Bot.Types           (ChannelSecret (..))
import           Line.Bot.Webhook.Events  as Events
import           Network.HTTP.Types       (HeaderName, hContentType)
import           Network.Wai              (lazyRequestBody,
                                           requestHeaders)
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server.Internal

-- | This type alias just specifies how webhook requests should be handled
type Webhook = LineReqBody '[JSON] Events :> Post '[JSON] NoContent

-- | Helper function that takes a handler to process 'Webhook' events:
--
-- > server :: Server Webhook
-- > server = webhook $ \case
-- >   EventMessage { message, replyToken } = handleMessage message replyToken
-- >   _                                    = return ()
webhook :: MonadIO m => (Event -> m a) -> Events -> m NoContent
webhook k Events{..} = forM_ events k >> return NoContent

-- | A Servant combinator that extracts the request body as a value of type a
-- and performs signature valiadation
data LineReqBody (contentTypes :: [*]) (a :: *)
  deriving (Typeable)

instance (AllCTUnrender list a, HasServer api context, HasContextEntry context ChannelSecret)
  => HasServer (LineReqBody list a :> api) context where

  type ServerT (LineReqBody list a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver
      = route (Proxy :: Proxy api) context $
          addBodyCheck subserver ctCheck bodyCheck
    where
      ctCheck = withRequest $ \request -> do
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy list) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f  -> return f

      bodyCheck f = withRequest $ \ request -> do
        rawBody <- liftIO $ lazyRequestBody request
        let signatureH = lookup hSignature $ requestHeaders request

        if validateReqBody signatureH rawBody
          then case f rawBody of
             Left e  -> delayedFailFatal err400 { errBody = cs e }
             Right v -> return v
          else delayedFailFatal err401

      channelSecret :: ChannelSecret
      channelSecret = getContextEntry context

      hSignature :: HeaderName
      hSignature = "X-Line-Signature"

      validateReqBody :: Maybe B.ByteString -> BL.ByteString -> Bool
      validateReqBody digest body = digest' == Just (SHA256.hmaclazy secret body)
        where
          digest' = Base64.decodeLenient <$> digest
          secret  = unChannelSecret channelSecret

