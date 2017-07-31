module Operations.VerifiedState
(
  -- Types
  Intent, VerifiedState, Outcome
  -- Functions
, update, resolve
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Types.Failure
import Types.Game

-- Represents an intent to modify the state of a game,
-- but can fail due to some specified reason
type Intent = Game -> Either Failure Game

-- Custom monad to represent a stateful computation that
-- can be verified against failures
type VerifiedState a = S.StateT Game (E.Except Failure) a

-- Tries to update the state with an intent.
-- If succesful, returns with a given value.
-- Otherwise, the reason is given instead.
update :: Intent -> a -> VerifiedState a
update intent val = S.get >>= fromIntent >>= S.put >> return val
  where fromIntent = lift . E.except . intent

-- The result of a verified state operation
type Outcome a = Either Failure (a, Game)

-- Resolves the outcome from running a verified state over a game
resolve :: VerifiedState a -> Game -> Outcome a
resolve state game = E.runExcept $ S.runStateT state game