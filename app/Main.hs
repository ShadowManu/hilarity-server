{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.Class

import Lib

import Operations.Intents

type Message = T.Text
type MessageChannel = STM.TChan Message

makeMessages :: IO MessageChannel
makeMessages = atomically newTChan

address = "127.0.0.1"
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting server on " ++ address ++ ":" ++ show port
  chan <- makeMessages

  serv <- async $ WS.runServer address port (app chan)
  wait serv

app :: MessageChannel -> WS.ServerApp
app chan pending = do
  putStrLn "Accepting incoming connection"
  conn <- WS.acceptRequest pending
  handle chan conn

handle :: MessageChannel -> WS.Connection -> IO ()
handle channel conn = do
  dupChannel <- atomically $ STM.dupTChan channel
  concurrently_ (receive dupChannel) (send dupChannel)

  where
    receive chan = do
      done <- async . forever $ do
        message <- WS.receiveData conn
        atomically $ STM.writeTChan chan message
      wait done

    send chan = do
      done <- async . forever $ do
        message <- atomically $ STM.readTChan chan
        WS.sendTextData conn message
      wait done