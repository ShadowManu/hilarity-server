{-# LANGUAGE OverloadedStrings #-}

module Deck
( Deck(Deck)
, testDeck
, decks
) where

import qualified Data.Map as M

import Card

data Deck = Deck { getBlacks :: M.Map CardId BlackCard, getWhites :: M.Map CardId WhiteCard } deriving Show

testDeck :: Deck
testDeck = decks !! 1

decks :: [Deck]
decks =
  [ Deck
      (M.fromList [ ("1", BlackCard "1" "Negra 1" 1)
                  , ("2", BlackCard "2" "Negra 2" 2)
                  , ("3", BlackCard "3" "Negra 3" 3)
                  ]
      )

      (M.fromList [ ("1", WhiteCard "1" "Blanca 1")
                  , ("2", WhiteCard "2" "Blanca 2")
                  , ("3", WhiteCard "3" "Blanca 3")
                  ]
      )
  ]