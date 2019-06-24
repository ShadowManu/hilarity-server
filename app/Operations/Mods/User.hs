{-# LANGUAGE OverloadedStrings #-}

module Operations.Mods.User
( addUser
) where

import Control.Lens
import Control.Monad (replicateM)
import Data.Text (Text)
import System.Random

import Operations.Mods
import Operations.Utils

import qualified Types.Card as Ca
import Types.Common
import qualified Types.Deck as D
import qualified Types.Game as G
import qualified Types.Hand as H
import qualified Types.State as S
import qualified Types.Users as U
import Types.State (State)
import Types.Failure

-- Add user to game
addUser :: UserId -> Mod State Text
addUser name = do
  exists <- use $ S.game . G.users . to (U.has name)
  assert (not exists) Failure
  S.game . G.users %= U.add name 
  return name
