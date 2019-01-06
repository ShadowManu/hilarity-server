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

-- Our error monad (base)
type Err = E.ExceptT Failure Identity

-- Our state monad
type St s m a = S.StateT s m a 

-- Our full monad. A stateful computation that can fail.
type Mod s a = St s Err a

-- Monad run function. Runs the failure-aware computatation with the given state
runMod :: Mod s a -> s -> Either Failure (a, s)
runMod mod state = E.runExcept $ S.runStateT mod state

runModWithLens :: Lens' s t -> Mod t a -> Mod s a
runModWithLens lens targetMod = do
  target <- use lens
  either withErr withVal $ runMod targetMod target
  where
    withErr err = lift . E.throwE $ err
    withVal (a, t) = do
      lens .= t
      return a

---- Operations
-- Add User
-- Start Round (distribute cards,  choose czar)
-- Play Card
-- End Round
-- Choose winner
-- Remove user
