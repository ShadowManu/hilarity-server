module Types.Hand
( Hand
, empty
, addCard
) where

import qualified Data.Set as S

import Types.Card

type Hand = S.Set CardId

empty :: Hand
empty = S.empty

addCard :: CardId -> Hand -> Hand
addCard = S.insert