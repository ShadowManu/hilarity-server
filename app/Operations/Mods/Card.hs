
module Operations.Mods.Card
( fillHand
) where

import Data.Monoid
import Data.Foldable
import Control.Lens
import Control.Monad (replicateM)
import System.Random

import Operations.Mods
import Operations.Mods.Common (rand)
import Operations.Utils

import Types.State (State)
import qualified Types.Card as C
import Types.Common
import qualified Types.Deck as D
import Types.Failure
import qualified Types.Game as G
import qualified Types.Hand as H
import qualified Types.Round as R
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

getCurrentRound :: Mod State R.Round
getCurrentRound = do
  rounds <- use $ S.game . G.rounds
  assert (not $ null rounds) $ RoundNotPlayable
  return $ head rounds

-- User tries to play a card
playRound :: UserId -> [C.CardId] -> Mod State ()
playRound user cards = do
  assertNumberOfCards
  assertCardsAvailable
  S.game . G.rounds . ix 0 %= R.play user cards

  where
    assertNumberOfCards = do
      round <- getCurrentRound
      black <- extract RoundNotPlayable $ round ^. R.black
      deck <- use $ S.game . G.deck . D.blackCards
      let holes = deck ^. to (D.get black) . C.blackHoles
      assert (holes == length cards) $ WrongNumberOfCards holes

    assertCardsAvailable = do
      hand <- use $ S.game . G.users . U.byId user
      let hasThemAll = getAll $ foldMap (\card -> All $ H.has card hand) cards
      assert (hasThemAll) $ CardNotAvailable cards
