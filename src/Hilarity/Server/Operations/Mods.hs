{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Hilarity.Server.Operations.Mods
  ( Mod,
    runMod,
    applyMod,
    throw,
  )
where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Data.Text as T
import Hilarity.Server.Types.Failure (Failure)
import qualified Hilarity.Server.Types.Game as G
import Hilarity.Server.Types.State

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

liftMod :: Lens' s t -> Mod t a -> Mod s a
liftMod lens targetMod = do
  target <- use lens
  either ifError ifSuccess $ runMod targetMod target
  where
    ifError err = lift $ E.throwE err
    ifSuccess (a, t) = do
      lens .= t
      return a

applyMod :: Mod s a -> TVar s -> STM (Either Failure a)
applyMod mod tState = do
  state <- readTVar tState
  either ifError ifSuccess $ runMod mod state
  where
    ifError err = return $ Left err
    ifSuccess (a, newState) = do
      writeTVar tState newState
      return $ Right a

throw :: Failure -> Mod s a
throw = lift . E.throwE