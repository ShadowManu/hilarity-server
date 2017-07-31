{-# LANGUAGE OverloadedStrings #-}

module Types.Deck
( Deck(Deck)
, newDeck
, makeDeck
, testDeck
) where

import qualified Data.Map as M
import qualified Data.Text as T

import Types.Card

data Deck = Deck { getBlacks :: M.Map CardId BlackCard, getWhites :: M.Map CardId WhiteCard } deriving Show

newDeck :: Deck
newDeck = Deck M.empty M.empty

makeDeck :: [(CardId, T.Text, Integer)] -> [(CardId, T.Text)] -> Deck
makeDeck blacks whites = Deck bMap wMap
  where
    bPair (id, con, holes) = (id, BlackCard id con holes)
    wPair (id, con) = (id, WhiteCard id con)
    bMap = M.fromList . map bPair $ blacks
    wMap = M.fromList . map wPair $ whites

testDeck :: Deck
testDeck = makeDeck
  [ ("1", "Primera", 1)
  , ("2", "Segunda", 1)
  , ("3", "Tercera", 2)
  ]
  [ ("1", "Primera")
  , ("2", "Segunda")
  , ("3", "Tercera")
  ]