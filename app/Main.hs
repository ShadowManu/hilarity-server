{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server (runServer)

address = "127.0.0.1"
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting server on " ++ address ++ ":" ++ show port
  runServer address port