{-# LANGUAGE TemplateHaskell #-}

module Types.Users
( Users
) where

import qualified Data.Map as M
-- import Data.Maybe
-- import qualified Data.Monoid as Mo
-- import qualified Data.Set as S
-- import qualified Data.Text as T
-- import Control.Lens

-- import Types.Card
import Types.Common
-- import qualified Types.Deck as Deck
import qualified Types.Hand as Hand
-- import Types.Round

newtype Users = Users (M.Map UserId Hand.Hand)