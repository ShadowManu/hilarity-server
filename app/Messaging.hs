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

import Prelude hiding ((!))

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forever, forM_)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import qualified Data.Text as T
import Control.Lens

import qualified Types.Common as C

type Recipient = C.UserId
type Response = T.Text

-- Ways a message must be casted
data Cast
  = Uni (Recipient, Response)
  | Multi [(Recipient, Response)]
  | Broad Response
  | Raw WS.Connection Response

data Messenger = Messenger
  { _cast :: TMVar Cast
  , _recipients :: TVar (M.Map C.UserId WS.Connection) }

makeLenses ''Messenger

new :: STM Messenger
new = do
  recipients <- newTVar M.empty
  cast <- newEmptyTMVar
  return $ Messenger cast recipients

add :: Recipient -> WS.Connection -> Messenger -> STM ()
add recipient connection messenger =
  modifyTVar
    (messenger ^. recipients)
    (at recipient ?~ connection)

send :: Cast -> Messenger -> STM ()
send c messenger = putTMVar (messenger ^. cast) c

startBroadcast :: Messenger -> IO ThreadId
startBroadcast messenger = forkIO . forever $ do
  (cast, rs) <- atomically $ do
    cast <- takeTMVar $ messenger ^. cast
    recipients <- readTVar $ messenger ^. recipients
    return (cast, recipients)

  case cast of
    Uni (r, response) ->
      WS.sendTextData (rs ! r) response

    Multi targets ->
      forM_ targets $ \(r, response) ->
        WS.sendTextData (rs ! r) response

    Broad response ->
      forM_ (M.elems rs) $ \connection ->
        WS.sendTextData connection response

    Raw connection response ->
      WS.sendTextData connection response