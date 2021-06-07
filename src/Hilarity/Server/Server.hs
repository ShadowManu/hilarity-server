module Hilarity.Server.Server
  ( runServer,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever, unless)
import qualified Data.Text as T
import qualified Hilarity.Server.Broker as Broker
import qualified Hilarity.Server.Events as Events
import Hilarity.Server.Operations.Mods (applyMod)
import Hilarity.Server.Operations.Mods.User (addUser)
import qualified Hilarity.Server.Types.Common as C
import qualified Hilarity.Server.Types.Failure as Failure
import Hilarity.Server.Types.State (State, newIO)
import qualified Network.WebSockets as WS
import qualified System.Random

type TState = TVar State

runServer :: String -> Int -> IO ()
runServer address port = do
  tState <- newIO >>= newTVarIO
  broker <- atomically Broker.new

  -- TODO close broadcast thread
  Broker.startBroadcast broker
  WS.runServer address port (app tState broker)

-- Application loop for each pending connection
app :: TState -> Broker.Broker -> WS.ServerApp
app tState broker pending = WS.acceptRequest pending >>= handle tState broker

handle :: TVar State -> Broker.Broker -> WS.Connection -> IO b
handle tState broker connection = registerUser >> receive
  where
    registerUser = do
      username <- WS.receiveData connection :: IO T.Text
      registered <- atomically $ do
        result <- applyMod (addUser username) tState
        -- TODO based on result, a better message must be built
        reply result username
      unless registered registerUser
      where
        reply (Left failure) _ = do
          let message = Broker.Raw connection $ Events.Failure failure
          Broker.send message broker
          return False
        reply (Right text) username = do
          Broker.add username connection broker
          let message = Broker.Uni (username, Events.Raw text)
          Broker.send message broker
          return True

    receive = forever $ do
      inbound <- WS.receiveData connection
      -- Temporarily send a broad message with the same message
      -- Next steps: use with applyMod
      let message = Broker.Broad $ Events.Raw inbound
      atomically $ Broker.send message broker