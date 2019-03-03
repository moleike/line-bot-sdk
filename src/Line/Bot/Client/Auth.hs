{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Line.Bot.Client.Auth
-- Copyright   : (c) Alexandre Moreno, 2019
-- License     : BSD3
-- Maintainer  : alexmorenocano@gmail.com
-- Stability   : experimental

module Line.Bot.Client.Auth where

import           Line.Bot.Endpoints                   (ChannelAuth)
import           Line.Bot.Types                       (ChannelToken)
import           Network.HTTP.Types                   (hAuthorization)
import           Servant.API                          (AuthProtect)
import           Servant.Client.Core.Internal.Auth    (AuthClientData,
                                                       AuthenticatedRequest,
                                                       mkAuthenticatedRequest)
import           Servant.Client.Core.Internal.Request (Request, addHeader)


type instance AuthClientData ChannelAuth = ChannelToken

type Auth = AuthenticatedRequest ChannelAuth

mkAuth :: ChannelToken -> Auth
mkAuth token = mkAuthenticatedRequest token addAuthorizationHeader
 where
  addAuthorizationHeader :: ChannelToken -> Request -> Request
  addAuthorizationHeader = addHeader hAuthorization
