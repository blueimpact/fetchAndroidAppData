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
import Text.Pretty.Simple

import TinyScheduler.Jobs
import TinyScheduler.SubJobs
import TinyScheduler.Time
import Data.Time
import GetAppDetails

main :: IO ()
main = mainWebSocketHandler


mainWebSocketHandler :: IO ()
mainWebSocketHandler = do
  db <- openDB

  let forkCronTask = forkIO $ cronTask db
  forkCronTask
  runEnv 3000 (app db)

cronTask db = do
  putStrLn $ ("Starting CronTasks" :: Text)
  let
    jobx x = makeJob 1 10000 (Hours 24) startTime (fetchDataJP db defFileStoreConfig)
      where
        -- 9am next day
        startTime = UTCTime (addDays 1 (utctDay x)) (9*60*60)
  getCurrentTime >>= (\x ->
      execSubJobs . convertJobIntoSubJobs x $ (jobx x)) >> return ()

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
      -- pPrint d
      let
          hReq = handleRequest handler d

      resp <- runHaskeyT hReq db defFileStoreConfig

      -- pPrint resp
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
