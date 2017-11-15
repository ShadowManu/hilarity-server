{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Operations.Mods
( Mod
, runMod
-- , addUser
) where

import qualified Data.Text as T
import Control.Monad.Trans.Class
import Control.Lens
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Types.Failure
import qualified Types.Game as G
import Types.State

-- Monad Definition

-- type Operation = State -> Either Failure (a, s)

-- Custom monad to represent a stateful computation that
-- modifies a state 's' getting result 'a' that can have a Failure.
type Mod s a = S.StateT s (E.Except Failure) a 

-- Monad run function. Runs the modification in the given state
runMod :: Mod s a -> s -> Either Failure (a, s)
runMod mod state = E.runExcept $ S.runStateT mod state

-- Utilities

runModWithLens :: Lens' s t -> Mod t a -> Mod s a
runModWithLens lens targetMod = do
  -- Bind source and target states
  source <- S.get
  let target = source ^. lens

  -- Run target mod
  case runMod targetMod target of
    -- If its a failure, propagate it
    Left f -> lift . E.except $ Left f
    -- If its successful, update target in source and return value
    Right (a, t) -> do
      S.put $ source & lens .~ target
      return a

---- Operations
-- Add User
-- Start Round (distribute cards,  choose czar)
-- Play Card
-- End Round
-- Choose winner
-- Remove user
