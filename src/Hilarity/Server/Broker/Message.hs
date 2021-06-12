module Hilarity.Server.Broker.Message
  ( Message (..),
  )
where

import Hilarity.Server.Events (OutboundEvent)
import qualified Hilarity.Server.Types.Common as Common
import qualified Network.WebSockets as WebSockets

-- Ways a message can be sent
data Message
  = Uni (Common.UserId, OutboundEvent)
  | Multi [(Common.UserId, OutboundEvent)]
  | Broad OutboundEvent
  | Raw WebSockets.Connection OutboundEvent
