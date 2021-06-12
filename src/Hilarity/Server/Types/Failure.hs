{-# LANGUAGE DeriveGeneric #-}

module Hilarity.Server.Types.Failure
  ( Failure (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hilarity.Server.Types.Card
import Hilarity.Server.Types.Common

data Failure
  = Failure {reason :: Maybe Text}
  | UserAlreadySignedIn UserId
  | CardNotAvailable [CardId]
  | WrongNumberOfCards Int
  | RoundNotPlayable
  deriving (Eq, Show, Generic)

instance FromJSON Failure

instance ToJSON Failure
