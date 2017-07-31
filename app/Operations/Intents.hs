module Operations.Intents where

import qualified Operations.VerifiedState as VS
import Types.Deck
import Types.Failure
import qualified Types.Game as G

addUser :: G.UserId -> VS.Intent
addUser id game = if id `G.inGame` game then Left Failure
                                else Right $ G.addUser id game
