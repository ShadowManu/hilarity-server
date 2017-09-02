module Types.State
( State
, newState
, newStateIO
) where

import Control.Applicative
import System.Random

import Types.Game

data State = State Game StdGen

newState :: StdGen -> State
newState = State newGame

newStateIO :: IO State
newStateIO = newState <$> newStdGen