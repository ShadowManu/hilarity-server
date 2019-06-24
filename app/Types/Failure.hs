module Types.Failure 
( Failure(..)
) where

import Types.Common

data Failure
  = Failure
  | UserAlreadySignedIn UserId
  deriving Show