module Server
( runServer
) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Control.Concurrent.Broadcast as Bc
import Control.Monad (forever, unless)
import Control.Lens
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified System.Random

import qualified Types.Common as C
import qualified Types.State as S
import qualified Messaging as Me
import qualified Operations.Mods as Mod
import qualified Operations.Mods.User as Mod

type UserList = M.Map C.UserId WS.Connection

runServer :: String -> Int -> IO ()
runServer address port = do
  tState <- S.newIO >>= atomically . newTVar
  messenger <- atomically Me.new

  -- TODO close broadcast thread
  Me.startBroadcast messenger
  WS.runServer address port (application tState messenger)

application :: TVar S.State -> Me.Messenger -> WS.ServerApp
application tState messenger pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  handleConnection tState messenger connection

handleConnection :: TVar S.State -> Me.Messenger -> WS.Connection -> IO ()
handleConnection tState messenger connection = do
  registerUser
  receive

  where
    registerUser = do
      username <- WS.receiveData connection :: IO T.Text

      success <- atomically $ do
        result <- Mod.applyMod (Mod.addUser username) tState

        case result of
          Left failure -> do
            let message = T.pack . show $ failure
            Me.send (Me.Raw connection message) messenger
            return False

          Right message -> do
            Me.add username connection messenger
            Me.send (Me.Uni (username, message)) messenger
            return True

      unless success registerUser

    receive = forever $ do
      message <- WS.receiveData connection
      atomically $ Me.send (Me.Broad message) messenger