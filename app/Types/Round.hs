{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Round
( Round(Round)
, black
, plays
, czar
, winner

, empty
, play
) where

-- import qualified Data.Monoid as Mo
import Data.Semigroup
import qualified Data.Map as M
import Control.Lens

import Types.Card
import Types.Common

data Round = Round
  { _black :: Maybe CardId
  , _plays :: M.Map UserId [CardId]
  , _czar :: Maybe UserId
  , _winner :: Maybe UserId }
  deriving (Eq, Show)

makeLenses ''Round

empty :: Round
empty = Round
  { _black = Nothing
  , _plays = M.empty
  , _czar = Nothing
  , _winner = Nothing }

play :: UserId -> [CardId] -> Round -> Round
play user cards = plays %~ M.insert user cards