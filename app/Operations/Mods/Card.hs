
module Operations.Mods.Card
( fillHand
) where

import Control.Lens
import Control.Monad (replicateM)
import System.Random

import Operations.Mods
import Operations.Mods.Common (rand)

import Types.State (State)
import qualified Types.Card as C
import Types.Common
import qualified Types.Deck as D
import qualified Types.Game as G
import qualified Types.Hand as H
import qualified Types.State as S
import qualified Types.Users as U

initialCards = 5

-- Obtain random white card
randWhiteCard :: Mod State C.CardId
randWhiteCard = do
  number <- rand
  whiteCards <- use $ S.game . G.deck . D.whiteCards
  return $ D.randomSelect number whiteCards

-- Obtain random black cards
randBlackCard :: Mod State C.CardId
randBlackCard = do
  number <- rand
  blackCards <- use $ S.game . G.deck . D.blackCards
  return $ D.randomSelect number blackCards

-- Deal a card to a player, indicating the given card
dealCard :: UserId -> Mod State C.CardId
dealCard id = do
  card <- randWhiteCard
  S.game . G.users . U.byId id %= H.add card
  return card

-- Fills a player hand with missing cards
fillHand :: UserId -> Mod State [C.CardId]
fillHand id = do
  current <- use $ S.game . G.users . U.byId id
  let missing = initialCards - (H.size current)
  replicateM missing $ dealCard id
