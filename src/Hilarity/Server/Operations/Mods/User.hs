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
import qualified Hilarity.Server.Types.Failure as F
import qualified Hilarity.Server.Types.Game as G
import qualified Hilarity.Server.Types.Hand as H
import Hilarity.Server.Types.State (State)
import qualified Hilarity.Server.Types.State as S
import qualified Hilarity.Server.Types.Users as U
import System.Random

-- Add user to game
addUser :: UserId -> Mod State UserId
addUser name = do
  exists <- use $ S.game . G.users . to (U.has name)
  assert (not exists) (F.UserAlreadySignedIn name)
  S.game . G.users %= U.add name
  return name
