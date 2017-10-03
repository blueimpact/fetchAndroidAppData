{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where


import Protolude
import Handler
import qualified Message

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Network.WebSockets

import Control.Monad.Haskey

main :: IO ()
main = mainWebSocketHandler


mainWebSocketHandler :: IO ()
mainWebSocketHandler = do
  db <- openDB

  forkCronTask db
  runEnv 3000 (app db)


app :: _  -> Application
app handlerStateRef dbConn =
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
