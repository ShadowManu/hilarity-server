{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hilarity.Server.Broker
  ( Message (..),
    Broker,
    new,
    add,
    send,
    startBroadcast,
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (forM_, forever)
import Data.Aeson (encode)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Text as T
import Hilarity.Server.Events (OutboundEvent)
import qualified Hilarity.Server.Types.Common as C
import qualified Network.WebSockets as WS
import Prelude hiding ((!))

type Target = C.UserId

type Response = OutboundEvent

-- Ways a message can be sent
data Message
  = Uni (Target, Response)
  | Multi [(Target, Response)]
  | Broad Response
  | Raw WS.Connection Response

type Connections = M.Map Target WS.Connection

data Broker = Broker
  { _connections :: TVar Connections,
    _queue :: TBQueue Message
  }

makeLenses ''Broker

queueSize = 10

new :: STM Broker
new = Broker <$> newTVar M.empty <*> newTBQueue queueSize

add :: Target -> WS.Connection -> Broker -> STM ()
add target connection broker =
  modifyTVar
    (broker ^. connections)
    (at target ?~ connection)

send :: Message -> Broker -> STM ()
send message broker = writeTBQueue (broker ^. queue) message

startBroadcast :: Broker -> IO ThreadId
startBroadcast broker = forkIO . forever $ do
  (msg, connections) <- atomically $ do
    msg <- readTBQueue $ broker ^. queue
    connections <- readTVar $ broker ^. connections
    return (msg, connections)

  case msg of
    Uni (target, response) ->
      WS.sendTextData (connections ! target) $ encode response
    Multi targets ->
      forM_ targets $ \(target, response) ->
        WS.sendTextData (connections ! target) $ encode response
    Broad response ->
      forM_ (M.elems connections) $ \connection ->
        WS.sendTextData connection $ encode response
    Raw connection response ->
      WS.sendTextData connection $ encode response