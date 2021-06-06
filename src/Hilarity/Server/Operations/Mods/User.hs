{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Operations.Mods.User
( addUser
) where

import Control.Lens
import Control.Monad (replicateM)
import Data.Text (Text)
import System.Random

import Hilarity.Server.Operations.Mods
import Hilarity.Server.Operations.Utils

import qualified Hilarity.Server.Types.Card as Ca
import Hilarity.Server.Types.Common
import qualified Hilarity.Server.Types.Deck as D
import qualified Hilarity.Server.Types.Game as G
import qualified Hilarity.Server.Types.Hand as H
import qualified Hilarity.Server.Types.State as S
import qualified Hilarity.Server.Types.Users as U
import Hilarity.Server.Types.State (State)
import Hilarity.Server.Types.Failure

-- Add user to game
addUser :: UserId -> Mod State Text
addUser name = do
  exists <- use $ S.game . G.users . to (U.has name)
  assert (not exists) (UserAlreadySignedIn name)
  S.game . G.users %= U.add name 
  return name
