{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Events
  ( InboundEvent (..),
    OutboundEvent (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import qualified Hilarity.Server.Types.Failure as F
import Network.WebSockets (WebSocketsData (fromLazyByteString))

-- >>> Data.Aeson.encode $ AuthUserSignIn "ShadowManu"
data InboundEvent
  = AuthUserSignIn {id :: T.Text}
  | NetworkKeepAlive
  deriving (Eq, Show, Generic)

instance FromJSON InboundEvent

instance ToJSON InboundEvent

data OutboundEvent
  = AuthUserSignInSuccess
  | AuthUserSignInFailure
  | Failure F.Failure
  | Raw T.Text
  deriving (Eq, Show, Generic)

instance FromJSON OutboundEvent

instance ToJSON OutboundEvent
