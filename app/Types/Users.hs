{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Users
( Users
, empty
, add
, byId
, has
) where

import qualified Data.Map as M
-- import Data.Maybe
-- import qualified Data.Monoid as Mo
-- import qualified Data.Set as S
-- import qualified Data.Text as T
import Control.Lens hiding (has)

-- import Types.Card
import Types.Common
-- import qualified Types.Deck as Deck
import qualified Types.Hand as Hand
-- import Types.Round

type Users = M.Map UserId Hand.Hand

empty :: Users
empty = M.empty

add :: UserId -> Users -> Users
add id us = M.insert id Hand.empty us

byId :: UserId -> Traversal' Users Hand.Hand
byId = ix

has :: UserId -> Users -> Bool
has = M.member