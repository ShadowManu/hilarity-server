{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hilarity.Server.Types.Game
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

import Hilarity.Server.Types.Card
import Hilarity.Server.Types.Common
import qualified Hilarity.Server.Types.Deck as Deck
import qualified Hilarity.Server.Types.Hand as Hand
import qualified Hilarity.Server.Types.Round as Round
import qualified Hilarity.Server.Types.Users as Users

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