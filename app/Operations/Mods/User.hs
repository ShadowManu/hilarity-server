{-# LANGUAGE OverloadedStrings #-}

module Operations.Mods.User
( addUser
) where

import Control.Lens
import Control.Monad (replicateM_)
-- import qualified Control.Monad.Trans.State.Lazy as S hiding (State)
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Random

import Operations.Mods
import Operations.Utils

import qualified Types.Card as Ca
import qualified Types.Common as Co
import qualified Types.Deck as D
import qualified Types.Game as G
import qualified Types.State as S
import Types.State (State)
import Types.Failure

initialCards = 5

-- Add user to game
addUser :: Co.UserId -> Mod State T.Text
addUser id = do
  game <- use S.game
  assert (not $ G.hasUser id game) Failure
  S.game %= G.addUser id 
  return "All ok!"

-- Obtain random int
rand :: Mod S.State Int
rand = do
  actualGen <- use S.gen
  let (value, newGen) = random actualGen
  S.gen .= newGen
  return value

-- Obtain random card
randWhiteCard :: Mod S.State Ca.CardId
randWhiteCard = do
  number <- rand
  whiteCards <- use $ S.game . G.deck . D.whiteCards
  return $ D.randomSelect number whiteCards

randBlackCard :: Mod S.State Ca.CardId
randBlackCard = do
  number <- rand
  blackCards <- use $ S.game . G.deck . D.blackCards
  return $ D.randomSelect number blackCards

fillHand :: Co.UserId -> Mod S.State ()
fillHand user = replicateM_ initialCards $ do
  card <- randWhiteCard
  modifying S.game $ G.dealCard card user