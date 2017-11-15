{-# LANGUAGE OverloadedStrings #-}

module Operations.Mods.User
( addUser
) where

import Control.Lens
import Control.Monad (replicateM_)
import qualified Control.Monad.Trans.State.Lazy as S hiding (State)
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Random

import Operations.Mods
import Operations.Utils

import qualified Types.Card as C
import qualified Types.Deck as D
import qualified Types.Game as G
import qualified Types.State as S
import Types.Failure

initialCards = 5

addUser :: G.UserId -> Mod S.State T.Text
addUser id = do
  game <- use S.game
  assert (not $ G.inGame id game) Failure
  modifying S.game $ G.addUser id 
  return "All ok!"

rand :: Mod S.State Int
rand = do
  actualGen <- use S.gen
  let (value, newGen) = random actualGen
  assign S.gen newGen
  return value

randWhiteCard :: Mod S.State C.CardId
randWhiteCard = do
  number <- rand
  whiteCards <- use $ S.game . G.deck . D.whiteCards
  let cardId = fst $ Map.elemAt (number `mod` Map.size whiteCards) whiteCards
  return cardId

randBlackCard :: Mod S.State C.CardId
randBlackCard = do
  number <- rand
  blackCards <- use $ S.game . G.deck . D.blackCards
  let cardId = fst $ Map.elemAt (number `mod` Map.size blackCards) blackCards
  return cardId

fillHand :: G.UserId -> Mod S.State ()
fillHand user = replicateM_ initialCards $ do
  card <- randWhiteCard
  modifying S.game $ G.addCard card user
