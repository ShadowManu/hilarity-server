module Hilarity.Server.Server
( runServer
) where

import Control.Concurrent.STM
import Control.Monad (forever, unless)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified System.Random

import qualified Hilarity.Server.Types.Common as C
import Hilarity.Server.Types.State (State, newIO)
import qualified Hilarity.Server.Messaging as Ms
import Hilarity.Server.Operations.Mods (applyMod)
import Hilarity.Server.Operations.Mods.User (addUser)

type TState = TVar State

runServer :: String -> Int -> IO ()
runServer address port = do
  tState <- newIO >>= atomically . newTVar
  messenger <- atomically Ms.new

  -- TODO close broadcast thread
  Ms.startBroadcast messenger
  WS.runServer address port (application tState messenger)

application :: TState -> Ms.Messenger -> WS.ServerApp
application tState messenger pending = do
  connection <- WS.acceptRequest pending
  handle tState messenger connection

handle :: TState -> Ms.Messenger -> WS.Connection -> IO ()
handle tState messenger connection = registerUser >> receive
  where
    registerUser = do
      username <- WS.receiveData connection :: IO T.Text

      registered <- atomically $ do
        result <- applyMod (addUser username) tState
        case result of
          Left failure -> do
            let message = T.pack . show $ failure
            Ms.send (Ms.Raw connection message) messenger
            return False

          Right message -> do
            Ms.add username connection messenger
            Ms.send (Ms.Uni (username, message)) messenger
            return True

      unless registered registerUser

    receive = forever $ do
      message <- WS.receiveData connection
      -- Temporarily send a broad message with the same message
      -- Next steps: use with applyMod
      atomically $ Ms.send (Ms.Broad message) messenger