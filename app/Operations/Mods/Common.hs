-- Add user, ...
-- Start round
-- Play white set ...
-- If round done, display plays
-- Choose winner, start round

module Operations.Mods.Common
( rand
) where

import Control.Lens
import System.Random

import Operations.Mods
import Types.State (State)
import qualified Types.State as S

-- Obtain random int
rand :: Mod State Int
rand = do
  actualGen <- use S.gen
  let (value, newGen) = random actualGen
  S.gen .= newGen
  return value