{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

import Lib

import qualified Operations.Intents as I
import qualified Operations.VerifiedState as VS
import qualified Types.Failure as F
import qualified Types.Game as G

type Message = T.Text
type MessageChannel = TChan Message

makeMessages :: IO MessageChannel
makeMessages = atomically newTChan

makeGame :: IO (TVar G.Game)
makeGame = atomically $ newTVar G.newGame

address = "127.0.0.1"
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting server on " ++ address ++ ":" ++ show port
  chan <- makeMessages
  gamevar <- makeGame

  serv <- async $ WS.runServer address port (app chan gamevar)
  wait serv

app :: MessageChannel -> TVar G.Game -> WS.ServerApp
app chan gamevar pending = do
  putStrLn "Accepting incoming connection"
  conn <- WS.acceptRequest pending
  handle chan gamevar conn

handle :: MessageChannel -> TVar G.Game -> WS.Connection -> IO ()
handle channel gamevar conn = do
  dupChannel <- atomically $ dupTChan channel
  concurrently_ (receive dupChannel) (send dupChannel)

  where
    receive :: MessageChannel -> IO ()
    receive chan = do
      done <- async . forever $ do
        -- Receive id
        message <- WS.receiveData conn :: IO T.Text

        atomically $ do
          -- Read game
          game <- readTVar gamevar 

          -- Get result
          let result = VS.resolve (VS.update (I.addUser message) ("Agregado " `T.append` message)) game :: Either F.Failure (T.Text, G.Game)

          case result of
            Left f -> writeTChan chan $ "Error" `T.append` T.pack (show f)
            Right (m, s) -> do
              writeTVar gamevar s
              writeTChan chan m
      wait done

    send chan = do
      done <- async . forever $ do
        message <- atomically $ readTChan chan
        WS.sendTextData conn message
      wait done