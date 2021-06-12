{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Operations.Mods.User
  ( addUser,
  )
where

import Control.Lens
import Control.Monad (replicateM)
import Data.Text (Text)
import Hilarity.Server.Operations.Mods (Mod)
import Hilarity.Server.Operations.Utils (assert)
import qualified Hilarity.Server.Types.Card as Ca
import Hilarity.Server.Types.Common (UserId)
import qualified Hilarity.Server.Types.Deck as D
import qualified Hilarity.Server.Types.Failure as Failure
import qualified Hilarity.Server.Types.Game as Game
import Hilarity.Server.Types.State (State)
import qualified Hilarity.Server.Types.State as State
import qualified Hilarity.Server.Types.Users as Users
import System.Random

-- Add user to game
addUser :: UserId -> Mod State UserId
addUser name = do
  -- We won't check if the user exists, since the same user being added again
  -- can be caused by a reconnection, and we will assume it as such.
  -- In the future, the authentication system will have to help in the server
  -- protecting other users being connected as other existing users
  -- exists <- use $ State.game . Game.users . to (Users.has name)
  -- assert (not exists) (Failure.UserAlreadySignedIn name)
  State.game . Game.users %= Users.add name
  return name
