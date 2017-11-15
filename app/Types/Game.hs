{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Game
(
  -- Common bindings
  UserId

  -- Round bindings
, Round(Round)
, plays
, czar
, winner
  
, newRound

  -- Hand bindings
, Hand
, newHand

  -- Game bindings
, Game
, deck
, users
, rounds

, newGame
, addCard
, addUser
, inGame
) where

import Data.DeriveTH
import Data.Derive.Monoid
import qualified Data.Map as M
import qualified Data.Monoid as Mo
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Lens

import Types.Card
import Types.Deck

-- Common bindings

type UserId = T.Text

-- Round bindings

data Round = Round { _plays :: M.Map UserId [WhiteCard], _czar :: Maybe UserId, _winner :: Maybe UserId } deriving Show

$(derive makeMonoid ''Round)
makeLenses ''Round

newRound :: Round
newRound = Mo.mempty

-- Hand bindings

type Hand = S.Set CardId

newHand :: Hand
newHand = Mo.mempty

-- Game bindings

data Game = Game { _deck :: Deck, _users :: M.Map UserId Hand, _rounds :: [Round] } deriving Show

$(derive makeMonoid ''Game)
makeLenses ''Game

newGame :: Game
newGame = Mo.mempty

addCard :: CardId -> UserId -> Game -> Game
addCard card user = users %~ M.adjust (S.insert card) user

addRound :: Round -> Game -> Game
addRound round = rounds %~ (round :)

addUser :: UserId -> Game -> Game
-- over :: ASetter s t a b -> (a -> b) -> s -> t 	-- Defined in ‘Control.Lens.Setter’
addUser id = users %~ M.insert id newHand

inGame :: UserId -> Game -> Bool
inGame id = M.member id . view users
-- inGame id game = M.member id (game ^. users)