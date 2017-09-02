module Operations.Utils
( assert
) where

import Types.Failure

assert :: Bool -> Failure -> Either Failure ()
assert cond fail
  | cond = Right ()
  | otherwise = Left fail