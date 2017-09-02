{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

import Lib

import qualified Operations.Mods as M
import qualified Types.Failure as F
import qualified Types.Game as G
import qualified Types.State as S

type Message = T.Text
type MessageChannel = TChan Message

makeMessages :: IO MessageChannel
makeMessages = atomically newTChan

makeState :: IO (TVar S.State)
makeState = do
  state <- S.newStateIO
  atomically $ newTVar state

address = "127.0.0.1"
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting server on " ++ address ++ ":" ++ show port
  chan <- makeMessages
  statevar <- makeState

  serv <- async $ WS.runServer address port (app chan statevar)
  wait serv

app :: MessageChannel -> TVar S.State -> WS.ServerApp
app chan statevar pending = do
  putStrLn "Accepting incoming connection"
  conn <- WS.acceptRequest pending
  handle chan statevar conn

handle :: MessageChannel -> TVar S.State -> WS.Connection -> IO ()
handle channel statevar conn = do
  dupChannel <- atomically $ dupTChan channel
  concurrently_ (receive dupChannel) (send dupChannel)

  where
    receive :: MessageChannel -> IO ()
    receive chan = do
      done <- async . forever $ do
        -- Receive id
        message <- WS.receiveData conn :: IO T.Text
        atomically $ do
          -- Read state
          state <- readTVar statevar

          -- Run mod
          let eith = M.runMod (M.addUser message) state

          case eith of
            Left fail -> writeTChan chan $ "Error: " `T.append` T.pack (show fail)
            Right (response, state) -> do
              writeTVar statevar state
              writeTChan chan response

      wait done

    send chan = do
      done <- async . forever $ do
        message <- atomically $ readTChan chan
        WS.sendTextData conn message
      wait done