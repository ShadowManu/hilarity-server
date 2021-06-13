{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Server
  ( runServer,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever, unless, when)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Hilarity.Server.Broker as Broker
import qualified Hilarity.Server.Broker.Message as Message
import qualified Hilarity.Server.Event.Inbound as Inbound
import qualified Hilarity.Server.Event.Outbound as Outbound
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
    registerUser = receiveEvent >>= tryRegister
      where
        tryRegister (Just (Inbound.AuthUserSignIn username)) = do
          result <- atomically $ do
            result <- applyMod (addUser username) tState
            when (isRight result) $ Broker.add username connection broker
            replyModWith (\o -> Message.Uni (username, o)) result
            return result
          when (isLeft result) registerUser
        tryRegister _ =
          atomically
            . notify
            . Message.Raw connection
            . Outbound.Failure
            $ Failure.Failure . Just . T.pack $ "Unexpected Event. Sign In first"

    receiveEvent = do
      byteData <- WS.receiveData connection :: IO ByteString
      let maybeEvent = decode byteData :: Maybe Inbound.Inbound
      maybe ifError ifDecoded maybeEvent
      where
        ifError = do
          atomically
            . notify
            . Message.Raw connection
            . Outbound.Failure
            . Failure.Failure
            $ Just "BAD JSON"
          return Nothing
        ifDecoded = return . return

    notify message = Broker.send message broker

    replyModWith _ (Left failure) =
      notify $
        Message.Raw connection $ Outbound.Failure failure
    replyModWith f (Right outbound) =
      notify $ f outbound

    receive = forever $ do
      inbound <- WS.receiveData connection
      -- Temporarily send a broad message with the same message
      -- Next steps: use with applyMod
      let message = Message.Broad $ Outbound.Raw inbound
      atomically $ Broker.send message broker