module Types.Hand
( Hand
, empty
, has
, add
, size
) where

import qualified Data.Set as S

import Types.Card

type Hand = S.Set CardId

empty :: Hand
empty = S.empty

has :: CardId -> Hand -> Bool
has = S.member

add :: CardId -> Hand -> Hand
add = S.insert

size :: Hand -> Int
size = S.size