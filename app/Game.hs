{-# LANGUAGE OverloadedStrings #-}

module Game
( Hand
, Round(Round)
, plays
, czar
, Game
, deck
, users
, rounds
, emptyGame
, UserId
) where

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

import Card
import Deck

type Hand = Set.Set WhiteCard

type UserId = T.Text
type Users = M.Map UserId Hand

data Round = Round { plays :: M.Map UserId [WhiteCard], czar :: Maybe UserId, winner :: Maybe UserId } deriving Show

emptyRound :: Round
emptyRound = Round { plays = M.empty, czar = Nothing, winner = Nothing }

data Game = Game { deck :: Deck, users :: Users, rounds :: [Round] } deriving Show

emptyGame :: Deck -> Game
emptyGame deck = Game { deck = deck, users = M.empty, rounds = [] }