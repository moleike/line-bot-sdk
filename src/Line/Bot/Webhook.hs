{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
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
  , LineReqBody
  , module Events
  )
where

import           Control.Monad.IO.Class   (liftIO)
import qualified Crypto.Hash.SHA256       as SHA256
import           Data.Aeson
import           Data.Aeson.Types
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
import           Network.Wai              (Request, lazyRequestBody,
                                           requestHeaders)
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server.Internal


type Webhook = LineReqBody '[JSON] Events :> Post '[JSON] NoContent

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
          then case (f rawBody) of
             Left e  -> delayedFailFatal err400 { errBody = cs e }
             Right v -> return v
          else delayedFailFatal err401

      channelSecret :: ChannelSecret
      channelSecret = getContextEntry context

      hSignature :: HeaderName
      hSignature = "X-Line-Signature"

      validateReqBody :: Maybe B.ByteString -> BL.ByteString -> Bool
      validateReqBody digest body = maybe False f digest'
        where
          digest' = Base64.decodeLenient <$> digest
          f = (== SHA256.hmaclazy (unChannelSecret channelSecret) body)

