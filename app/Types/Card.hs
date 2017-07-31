module Types.Card
( CardId
, BlackCard(BlackCard)
, WhiteCard(WhiteCard)
) where

import qualified Data.Map as M
import qualified Data.Text as T

type CardId = T.Text

data BlackCard = BlackCard { blackId :: CardId, blackContent :: T.Text, blackHoles :: Integer } deriving Show

data WhiteCard = WhiteCard { whiteId :: CardId, whiteContent :: T.Text } deriving Show