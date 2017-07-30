{-# LANGUAGE OverloadedStrings #-}

module Actions
( Action
, Failure(Failure)
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Data.Map as M
import qualified Data.Set as Set

import Game
import Deck

data Action = UserConnected 

data Result = Result deriving Show

data Failure = Failure deriving Show

-- Represents an intent to modify the state of a game,
-- but can fail due to some specified reason
type Intent = Game -> Either Failure Game

-- Custom monad to represent a stateful computation that
-- can be verified against failures
type VerifiedState a = S.StateT Game (E.Except Failure) a

-- Tries to update the state with an intent.
-- If succesful, returns with a given value.
-- Otherwise, the reason is given instead.
update :: Intent -> a -> VerifiedState a
update intent val = S.get >>= fromIntent >>= S.put >> return val
  where fromIntent = lift . E.except . intent

type Outcome a = Either Failure (a, Game)

resolve :: VerifiedState a -> Game -> Outcome a
resolve state game = E.runExcept $ S.runStateT state game

-- TODO MOVE
-- 
userJoined :: UserId -> Game -> Bool
userJoined id game = M.member id (users game)

emptyHand :: Hand
emptyHand = Set.empty

aggregateUser :: UserId -> Intent
aggregateUser id game
  | userJoined id game = Left Failure
  | otherwise = Right $ game { users = M.insert id emptyHand $ users game }