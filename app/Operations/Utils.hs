module Operations.Utils
( assert
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Types.Failure
import Types.Game
import Types.State

assert :: Bool -> Failure -> E.Except Failure ()
assert cond fail
  | cond = return ()
  | otherwise = E.throwE fail