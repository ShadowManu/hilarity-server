{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging
( Cast (Uni, Multi, Broad, Raw)
, Messenger
, new
, add
, send
, startBroadcast
) where

import Control.Concurrent (forkIO, ThreadId)
import qualified Control.Concurrent.Broadcast as Bc
import Control.Monad (forever, forM_)
import qualified Data.Map as M
import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import qualified Data.Text as T
import Control.Lens

import qualified Types.Common as C
import qualified Types.State as S
import qualified Types.Game as G

type Recipient = C.UserId
type Response = T.Text

data Cast
  = Uni (Recipient, Response)
  | Multi [(Recipient, Response)]
  | Broad Response
  | Raw WS.Connection Response

data Messenger = Messenger
  { _cast :: TMVar Cast
  , _recipients :: TVar (M.Map C.UserId WS.Connection)
  }

makeLenses ''Messenger

new :: STM Messenger
new = do
  recipients <- newTVar M.empty
  cast <- newEmptyTMVar
  return $ Messenger { _recipients = recipients, _cast = cast }

add :: Recipient -> WS.Connection -> Messenger -> STM ()
add recipient connection messenger =
  modifyTVar (messenger ^. recipients) (at recipient ?~ connection)

send :: Cast -> Messenger -> STM ()
send c messenger = putTMVar (messenger ^. cast) c

startBroadcast :: Messenger -> IO ThreadId
startBroadcast messenger = forkIO . forever $ do
  (cast, recipients) <- atomically $ do
    cast <- takeTMVar . (^. cast) $ messenger
    recipients <- readTVar . (^. recipients) $ messenger
    return (cast, recipients)

  case cast of
    Uni (recipient, response) -> do
      let connection = (M.!) recipients recipient
      WS.sendTextData connection response

    Multi targets ->
      forM_ targets $ \(recipient, response) ->
        let connection = (M.!) recipients recipient
        in WS.sendTextData connection response

    Broad response ->
      forM_ (M.elems recipients) $ \connection ->
        WS.sendTextData connection response

    Raw connection response ->
      WS.sendTextData connection response