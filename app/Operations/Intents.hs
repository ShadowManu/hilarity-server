module Operations.Intents where

import qualified Operations.VerifiedState as VS
import Types.Deck
import Types.Failure
import qualified Types.Game as G

assert :: Bool -> Failure -> Either Failure ()
assert cond fail
  | cond = Right ()
  | otherwise = Left fail

addUser :: G.UserId -> VS.Intent
addUser id game = do
  assert (not $ G.inGame id game) Failure
  return $ G.addUser id game
