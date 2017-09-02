module Types.State
( State(State)
, getGame
, getGen
, newState
, newStateIO
) where

import Control.Applicative
import System.Random

import Types.Game

data State = State { getGame :: Game, getGen :: StdGen } deriving Show

newState :: StdGen -> State
newState = State newGame

newStateIO :: IO State
newStateIO = newState <$> newStdGen