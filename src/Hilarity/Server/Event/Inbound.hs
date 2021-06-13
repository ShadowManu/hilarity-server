{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Event.Inbound
  ( Inbound (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Inbound
  = AuthUserSignIn {userId :: T.Text}
  | NetworkKeepAlive
  deriving (Eq, Show, Generic)

instance FromJSON Inbound

instance ToJSON Inbound