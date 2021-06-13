{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Event.Outbound
  ( Outbound (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import qualified Hilarity.Server.Types.Failure as F

data Outbound
  = AuthUserSignInSuccess {id2 :: T.Text}
  | AuthUserSignInFailure {other :: Int, id2 :: T.Text}
  | Failure F.Failure
  | Raw T.Text
  deriving (Eq, Show, Generic)

instance FromJSON Outbound

instance ToJSON Outbound
