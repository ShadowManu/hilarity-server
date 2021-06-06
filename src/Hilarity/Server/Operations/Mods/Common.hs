-- Add user, ...
-- Start round
-- Play white set ...
-- If round done, display plays
-- Choose winner, start round

module Hilarity.Server.Operations.Mods.Common
( rand
) where

import Control.Lens
import System.Random

import Hilarity.Server.Operations.Mods
import Hilarity.Server.Types.State (State)
import qualified Hilarity.Server.Types.State as S

-- Obtain random int
rand :: Mod State Int
rand = do
  actualGen <- use S.gen
  let (value, newGen) = random actualGen
  S.gen .= newGen
  return value