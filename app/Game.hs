{-# LANGUAGE OverloadedStrings #-}

module Game
( Game
) where

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

import Card
import Deck

type Hand = Set.Set WhiteCard

type UserId = T.Text
type Users = M.Map UserId Hand

data Round = Round { plays :: M.Map UserId [WhiteCard], czar :: UserId, winner :: UserId }

type Rounds = [Round]

data Game = Game { deck :: Deck, users :: Users, rounds :: [Round] }