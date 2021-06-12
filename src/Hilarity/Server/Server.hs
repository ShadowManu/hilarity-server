{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Server
  ( runServer,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import qualified Hilarity.Server.Broker as Broker
import qualified Hilarity.Server.Broker.Message as Message
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
      maybeUsername <- receiveEvent
      case maybeUsername of
        (Just (Events.AuthUserSignIn username)) -> do
          registered <- atomically $ do
            result <- applyMod (addUser username) tState
            -- TODO based on result, a better message must be built
            reply result username
          unless registered registerUser
        _ -> atomically $ Broker.send message broker
          where
            message =
              Message.Raw connection $
                Events.Failure
                  (Failure.Failure . Just . T.pack $ "Unexpected Event")
      where
        reply (Left failure) _ = do
          notify $
            Message.Raw connection $ Events.Failure failure
          return False
        reply (Right text) username = do
          Broker.add username connection broker
          let message = Message.Uni (username, Events.Raw text)
          notify message
          return True

    receiveEvent = do
      byteData <- WS.receiveData connection :: IO ByteString
      let maybeEvent = decode byteData :: Maybe Events.InboundEvent
      maybe ifError ifDecoded maybeEvent
      where
        ifError = do
          atomically
            . notify
            . Message.Raw connection
            . Events.Failure
            . Failure.Failure
            $ Just "BAD JSON"
          return Nothing
        ifDecoded = return . return

    notify message = Broker.send message broker

    receive = forever $ do
      inbound <- WS.receiveData connection
      -- Temporarily send a broad message with the same message
      -- Next steps: use with applyMod
      let message = Message.Broad $ Events.Raw inbound
      atomically $ Broker.send message broker