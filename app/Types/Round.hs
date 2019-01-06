{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Round
( Round(Round)
) where

-- import qualified Data.Monoid as Mo
import Data.Semigroup
import qualified Data.Map as M
import Control.Lens

import Types.Card
import Types.Common

data Round = Round
  { _plays :: M.Map UserId [WhiteCard]
  , _czar :: Maybe UserId
  , _winner :: Maybe UserId }
  deriving (Eq, Show)

makeLenses ''Round

empty :: Round
empty = Round
  { _plays = M.empty
  , _czar = Nothing
  , _winner = Nothing }