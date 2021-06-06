
module Hilarity.Server.Operations.Mods.Card
( fillHand
) where

import Data.Monoid
import Data.Foldable
import Control.Lens
import Control.Monad (replicateM)
import System.Random

import Hilarity.Server.Operations.Mods
import Hilarity.Server.Operations.Mods.Common (rand)
import Hilarity.Server.Operations.Utils

import Hilarity.Server.Types.State (State)
import qualified Hilarity.Server.Types.Card as C
import Hilarity.Server.Types.Common
import qualified Hilarity.Server.Types.Deck as D
import Hilarity.Server.Types.Failure
import qualified Hilarity.Server.Types.Game as G
import qualified Hilarity.Server.Types.Hand as H
import qualified Hilarity.Server.Types.Round as R
import qualified Hilarity.Server.Types.State as S
import qualified Hilarity.Server.Types.Users as U

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
