{-# LANGUAGE TemplateHaskell #-}

module Types.Card
( CardId

, BlackCard(BlackCard)
, blackId
, blackContent
, blackHoles

, WhiteCard(WhiteCard)
, whiteId
, whiteContent

) where

import qualified Data.Set as M
import qualified Data.Text as T
import Control.Lens

type CardId = T.Text

data BlackCard = BlackCard { _blackId :: CardId, _blackContent :: T.Text, _blackHoles :: Integer } deriving Show

data WhiteCard = WhiteCard { _whiteId :: CardId, _whiteContent :: T.Text } deriving (Eq, Ord, Show)

makeLenses ''BlackCard
makeLenses ''WhiteCard