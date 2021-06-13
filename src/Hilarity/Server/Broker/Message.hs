module Hilarity.Server.Broker.Message
  ( Message (..),
  )
where

import Hilarity.Server.Event.Outbound (Outbound)
import qualified Hilarity.Server.Types.Common as Common
import qualified Network.WebSockets as WebSockets

-- Ways a message can be sent
data Message
  = Uni (Common.UserId, Outbound)
  | Multi [(Common.UserId, Outbound)]
  | Broad Outbound
  | Raw WebSockets.Connection Outbound
