{-# LANGUAGE TemplateHaskell #-}

module Types.State
( Some(Some)
, State(State)
, game
, gen

, new
, newIO
) where

import Control.Applicative
import Control.Lens
import System.Random (StdGen, newStdGen)

import qualified Types.Game as Game

data Some = Some

data State = State
  { _game :: Game.Game
  , _gen :: StdGen }
  deriving Show

makeLenses ''State

new :: StdGen -> State
new = State Game.empty

newIO :: IO State
newIO = new <$> newStdGen