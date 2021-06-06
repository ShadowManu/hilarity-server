{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Types.Users
( Users
, empty
, add
, byId
, has
) where

import qualified Data.Map as M
import Control.Lens hiding (has)

import Hilarity.Server.Types.Common
import qualified Hilarity.Server.Types.Hand as Hand

type Users = M.Map UserId Hand.Hand

empty :: Users
empty = M.empty

add :: UserId -> Users -> Users
add id us = M.insert id Hand.empty us

byId :: UserId -> Traversal' Users Hand.Hand
byId = ix

has :: UserId -> Users -> Bool
has = M.member