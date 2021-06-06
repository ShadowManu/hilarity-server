{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hilarity.Server.Types.Deck
( Deck(Deck)
, SubDeck
, blackCards
, whiteCards

, empty
, size
, get
, randomSelect
, fromLists
, example
) where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Lens

import Hilarity.Server.Types.Card

type SubDeck a = M.Map CardId a

data Deck = Deck
  { _blackCards :: SubDeck BlackCard
  , _whiteCards :: SubDeck WhiteCard }
  deriving Show

makeLenses ''Deck

empty :: Deck
empty = Deck M.empty M.empty

size :: SubDeck a -> Int
size = M.size

get :: CardId -> SubDeck a -> a
get card sub = (M.!) sub card

randomSelect :: Int -> SubDeck a -> CardId
randomSelect num sub = fst $ M.elemAt index sub
  where index = num `mod` size sub

orderedId :: Int -> SubDeck a -> CardId
orderedId index = fst . M.elemAt index

fromLists :: [(CardId, T.Text, Int)] -> [(CardId, T.Text)] -> Deck
fromLists blacks whites = Deck bMap wMap
  where
    bPair (id, con, holes) = (id, BlackCard id con holes)
    wPair (id, con) = (id, WhiteCard id con)
    bMap = M.fromList . map bPair $ blacks
    wMap = M.fromList . map wPair $ whites

example :: Deck
example = fromLists
  [ ("1", "Primera", 1)
  , ("2", "Segunda", 1)
  , ("3", "Tercera", 2)
  ]
  [ ("1", "Primera")
  , ("2", "Segunda")
  , ("3", "Tercera")
  ]