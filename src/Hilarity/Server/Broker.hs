{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hilarity.Server.Broker
  ( Broker,
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
import qualified Data.Map as Map
import qualified Data.Text as Text
import Hilarity.Server.Broker.Message (Message)
import qualified Hilarity.Server.Broker.Message as Message
import Hilarity.Server.Events (OutboundEvent)
import qualified Hilarity.Server.Types.Common as Common
import qualified Network.WebSockets as WebSockets
import Prelude hiding ((!))

type Connections = Map.Map Common.UserId WebSockets.Connection

data Broker = Broker
  { _connections :: TVar Connections,
    _queue :: TBQueue Message
  }

makeLenses ''Broker

queueSize = 10

new :: STM Broker
new = Broker <$> newTVar Map.empty <*> newTBQueue queueSize

add :: Common.UserId -> WebSockets.Connection -> Broker -> STM ()
add userId connection broker =
  modifyTVar
    (broker ^. connections)
    (at userId ?~ connection)

send :: Message.Message -> Broker -> STM ()
send message broker = writeTBQueue (broker ^. queue) message

startBroadcast :: Broker -> IO ThreadId
startBroadcast broker = forkIO . forever $ do
  (msg, connections) <- atomically $ do
    msg <- readTBQueue $ broker ^. queue
    connections <- readTVar $ broker ^. connections
    return (msg, connections)

  case msg of
    Message.Uni (userId, response) -> do
      let connection = connections ! userId
      let payload = encode response
      WebSockets.sendTextData connection payload
    Message.Multi targets ->
      forM_ targets $ \(userId, response) -> do
        let connection = connections ! userId
        let payload = encode response
        WebSockets.sendTextData connection payload
    Message.Broad response ->
      forM_ (Map.elems connections) $ \connection -> do
        let payload = encode response
        WebSockets.sendTextData connection payload
    Message.Raw connection response -> do
      let payload = encode response
      WebSockets.sendTextData connection payload