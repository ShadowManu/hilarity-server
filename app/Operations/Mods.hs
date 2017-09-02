module Operations.Mods
( Mod
, runMod
, addUser
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Types.Failure
import qualified Types.Game as G
import Types.State
import Operations.Utils

-- Monad Definition

-- Custom monad to represent a stateful computation that
-- modifies a state 's' getting result 'a' that can have a Failure.
type Mod s a = S.StateT s (E.Except Failure) a 

-- Monad run function. Runs the modification in the given state
runMod :: Mod s a -> s -> Either Failure (a, s)
runMod mod state = E.runExcept $ S.runStateT mod state

-- Utilities

forGame :: Mod G.Game a -> Mod State a
forGame gameMod = do
  (State game gen) <- S.get
  (result, newGame) <- lift . E.except $ runMod gameMod game
  S.put $ State newGame gen
  return result

modifyGame :: G.Game -> State -> State
modifyGame newGame (State game gen) = State newGame gen

-- Mod Operations

addUser :: G.UserId -> Mod State String
addUser id = do
  game <- S.gets getGame
  lift $ assert (not $ G.inGame id game) Failure
  S.modify . modifyGame $ G.addUser id game
  return "All ok!"

---- Operations
-- Add User
-- Start Round (distribute cards,  choose czar)
-- Play Card
-- End Round
-- Choose winner
-- Remove user
