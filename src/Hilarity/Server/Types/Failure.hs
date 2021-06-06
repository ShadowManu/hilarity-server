module Hilarity.Server.Types.Failure 
( Failure(..)
) where

import Hilarity.Server.Types.Common
import Hilarity.Server.Types.Card

data Failure
  = Failure
  | UserAlreadySignedIn UserId
  | CardNotAvailable [CardId]
  | WrongNumberOfCards Int
  | RoundNotPlayable
  deriving Show