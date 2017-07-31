{-# LANGUAGE OverloadedStrings #-}

module Types.Game
(
  -- Common Bindings
  UserId
  -- Game bindings
, Game
, deck
, users
, rounds

, newGame
, addUser
, newHand
  -- Round bindings
, Round(Round)
, plays
, czar
  
, newRound
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Types.Card
import Types.Deck

-- Common bindings

type UserId = T.Text

-- Game bindings

data Game = Game { deck :: Deck, users :: M.Map UserId Hand, rounds :: [Round] } deriving Show

newGame :: Game
newGame = Game { deck = newDeck, users = M.empty, rounds = [] }

addUser :: UserId -> Game -> Game
addUser id game = game { users = M.insert id newHand (users game) }

type Hand = S.Set WhiteCard

newHand :: Hand
newHand = S.empty

-- Round Bindings

data Round = Round { plays :: M.Map UserId [WhiteCard], czar :: Maybe UserId, winner :: Maybe UserId } deriving Show

newRound :: Round
newRound = Round { plays = M.empty, czar = Nothing, winner = Nothing }