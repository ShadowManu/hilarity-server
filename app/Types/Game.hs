{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game
( Game
, deck
, users
, rounds

, empty
, dealCard
, addRound
, addUser
, userHand
, hasUser
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
import Types.Round
import Types.Users

data Game = Game
  { _deck :: Deck.Deck
  , _users :: Users
  , _rounds :: [Round] }
  deriving Show

makeLenses ''Game

empty :: Game
empty = Game
  { _deck = Deck.empty
  , _users = M.empty
  , _rounds = [] }

dealCard :: CardId -> UserId -> Game -> Game
dealCard card user = users . ix user %~ Hand.add card

addRound :: Round -> Game -> Game
addRound round = rounds %~ (round :)

addUser :: UserId -> Game -> Game
addUser user = users . at user ?~ Hand.empty

userHand :: UserId -> Game -> Maybe Hand.Hand
userHand user = preview $ users . ix user

hasUser :: UserId -> Game -> Bool
hasUser user = isJust . userHand user