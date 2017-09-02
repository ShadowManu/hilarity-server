module Operations.Mods
( Mod
, runMod
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Types.Failure
import Types.Game
import Types.State

-- Monad Definition

-- Custom monad to represent a stateful computation that
-- modifies a state 's' getting result 'a' that can have a Failure.
type Mod s a = S.StateT s (E.Except Failure) a 

-- Monad run function. Runs the modification in the given state
runMod :: Mod s a -> s -> Either Failure (a, s)
runMod mod state = E.runExcept $ S.runStateT mod state

-- Mod Operations
  
---- Operations
-- Add User
-- Start Round (distribute cards,  choose czar)
-- Play Card
-- End Round
-- Choose winner
-- Remove user
  
-- TODO REDO
-- addUser :: G.UserId -> VS.Intent
-- addUser id game = do
--   assert (not $ G.inGame id game) Failure
--   return $ G.addUser id game