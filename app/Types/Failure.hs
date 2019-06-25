module Types.Failure 
( Failure(..)
) where

import Types.Common
import Types.Card

data Failure
  = Failure
  | UserAlreadySignedIn UserId
  | CardNotAvailable [CardId]
  | WrongNumberOfCards Int
  | RoundNotPlayable
  deriving Show