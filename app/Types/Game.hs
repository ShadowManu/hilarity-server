{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game
( Game
, deck
, users
, rounds

, empty
) where

import Data.Maybe
import qualified Data.Monoid as Mo
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Lens

import Types.Card
import Types.Common
import qualified Types.Deck as Deck
import qualified Types.Hand as Hand
import qualified Types.Round as Round
import qualified Types.Users as Users

data Game = Game
  { _deck :: Deck.Deck
  , _users :: Users.Users
  , _rounds :: [Round.Round] }
  deriving Show

makeLenses ''Game

empty :: Game
empty = Game
  { _deck = Deck.empty
  , _users = Users.empty
  , _rounds = [] }