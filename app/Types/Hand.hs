module Types.Hand
( Hand
, empty
, add
, size
) where

import qualified Data.Set as S

import Types.Card

type Hand = S.Set CardId

empty :: Hand
empty = S.empty

add :: CardId -> Hand -> Hand
add = S.insert

size :: Hand -> Int
size = S.size