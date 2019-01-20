module Server
( runServer
) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Control.Concurrent.Broadcast as Bc
import Control.Monad (forever, when)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified System.Random

import qualified Types.Common as C
import qualified Types.State as S
import qualified Operations.Mods as Mod
import qualified Operations.Mods.User as Mod

import Control.Lens
import Data.Either
import qualified Data.Map as M

type UserList = M.Map C.UserId WS.Connection

runServer :: String -> Int -> IO ()
runServer address port = do
  tState <- S.newIO >>= atomically . newTVar
  tListeners <- atomically $ newTVar M.empty
  broadcast <- Bc.new
  WS.runServer address port (application tState tListeners broadcast)

application :: TVar S.State -> TVar UserList -> Bc.Broadcast T.Text -> WS.ServerApp
application tState tListeners broadcast pendingConnection = do
  putStrLn "Accepting incoming connection"

  connection <- WS.acceptRequest pendingConnection
  handleConnection tState tListeners broadcast connection

handleConnection :: TVar S.State -> TVar UserList -> Bc.Broadcast T.Text -> WS.Connection -> IO ()
handleConnection tState tListeners broadcast connection = do
  registerUser
  concurrently_ receive send

  where
    registerUser = do
      username <- WS.receiveData connection :: IO T.Text

      -- Try modding state and adding listener
      result <- atomically $ do
        res <- Mod.applyMod (Mod.addUser username) tState
        when (isRight res) $ modifyTVar tListeners (ix username .~ connection)
        return res

      case result of
        -- If it fails, report and retry
        Left failure -> do
          WS.sendTextData connection . T.pack . show $ failure
          registerUser

        -- If a success, report and continue
        Right text ->
          WS.sendTextData connection text

    receive = forever $ do
      putStrLn "receive loop"
      message <- WS.receiveData connection
      Bc.signal broadcast message

    send = forever $ do
      putStrLn "send loop"
      message <- Bc.listen broadcast
      WS.sendTextData connection message