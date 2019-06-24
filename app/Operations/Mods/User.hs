{-# LANGUAGE OverloadedStrings #-}

module Operations.Mods.User
( addUser
, rand
, randWhiteCard
, randBlackCard
, dealCard
, fillHand
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

initialCards = 5

-- Add user to game
addUser :: UserId -> Mod State Text
addUser name = do
  exists <- use $ S.game . G.users . to (U.has name)
  assert exists Failure
  S.game . G.users %= U.add name 
  return "All ok!"

-- Obtain random int
rand :: Mod S.State Int
rand = do
  actualGen <- use S.gen
  let (value, newGen) = random actualGen
  S.gen .= newGen
  return value

-- Obtain random white card
randWhiteCard :: Mod State Ca.CardId
randWhiteCard = do
  number <- rand
  whiteCards <- use $ S.game . G.deck . D.whiteCards
  return $ D.randomSelect number whiteCards

-- Obtain random black cards
randBlackCard :: Mod State Ca.CardId
randBlackCard = do
  number <- rand
  blackCards <- use $ S.game . G.deck . D.blackCards
  return $ D.randomSelect number blackCards

-- Deal a card to a player, indicating the given card
dealCard :: UserId -> Mod State Ca.CardId
dealCard id = do
  card <- randWhiteCard
  S.game . G.users . U.byId id %= H.add card
  return card

-- Fills a player hand with missing cards
fillHand :: UserId -> Mod State [Ca.CardId]
fillHand id = do
  current <- use $ S.game . G.users . U.byId id
  let missing = initialCards - (H.size current)
  replicateM missing $ dealCard id