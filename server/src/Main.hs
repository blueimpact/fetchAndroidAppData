{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where


import Protolude
import Handler
import DB
import qualified Message

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Network.WebSockets
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.WebSocket.Server

import Control.Monad.Haskey
import Control.Concurrent

main :: IO ()
main = mainWebSocketHandler


mainWebSocketHandler :: IO ()
mainWebSocketHandler = do
  db <- openDB

  let forkCronTask = forkIO $ putStrLn ("Started fork task" :: Text)
  forkCronTask
  runEnv 3000 (app db)


app :: ConcurrentDb Schema -> Application
app db =
  websocketsOr defaultConnectionOptions wsApp backupApp
  where
    -- wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- acceptRequest pending_conn
      loop conn

    loop conn = do
      d <- receiveData conn
      print d
      let
          hReq = handleRequest handler d

      resp <- runHaskeyT hReq db defFileStoreConfig

      print resp
      sendBinaryData conn resp
      loop conn

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

handler :: HandlerWrapper HandlerM Message.AppRequest
handler = HandlerWrapper $
  h getListOfRankings
  :<&> h getDetailsForRankingList
  :<&> h getHtmlForRankingList

  where
  h :: (WebSocketMessage Message.AppRequest a, Monad m)
    => (a -> m (ResponseT Message.AppRequest a))
    -> Handler m Message.AppRequest a
  h = makeHandler
