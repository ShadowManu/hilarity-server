{-# LANGUAGE TemplateHaskell #-}

module Types.State
( State(State)
, game
, gen

, newState
, newStateIO
) where

import Control.Applicative
import Control.Lens
import System.Random

import Types.Game

data State = State { _game :: Game, _gen :: StdGen } deriving Show

makeLenses ''State

newState :: StdGen -> State
newState = State newGame

newStateIO :: IO State
newStateIO = newState <$> newStdGen