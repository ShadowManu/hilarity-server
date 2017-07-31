{-# LANGUAGE OverloadedStrings #-}

module Actions
( Action
) where

import qualified Data.Map as M
import qualified Data.Set as Set

import qualified Operations.VerifiedState as VS
import Types.Failure
import Types.Game
import Types.Deck

data Action = UserConnected 

-- TODO MOVE
userJoined :: UserId -> Game -> Bool
userJoined id game = M.member id (users game)

aggregateUser :: UserId -> VS.Intent
aggregateUser id game
  | userJoined id game = Left Failure
  | otherwise = Right $ addUser id game