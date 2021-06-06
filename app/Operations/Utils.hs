module Operations.Utils
( assert
, extract
) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import Operations.Mods

import Types.Failure
import Types.Game
import Types.State

assert :: Bool -> Failure -> Mod s ()
assert cond fail
  | cond = return ()
  | otherwise = lift $ E.throwE fail

extract :: Failure -> Maybe a -> Mod s a
extract _ (Just a) = return a
extract fail Nothing = throw fail