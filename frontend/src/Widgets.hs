module Widgets where

import Protolude hiding (link, (&))
import Reflex.Dom
import Message

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message

type AppMonadT t m = WithWebSocketT Message.AppRequest t m
type AppMonad t m = (MonadWidget t m)

topWidget
  :: AppMonad t m => AppMonadT t m ()
topWidget = do
  ev <- getPostBuild
  rankingLists <- getWebSocketResponse $ GetListOfRankings <$ ev
  
  let latestRankingEv = fmapMaybe headMay rankingLists
  appHtmlPage latestRankingEv

appHtmlPage
  :: AppMonad t m
  => Event t RankingListId
  -> AppMonadT t m ()
appHtmlPage rankListId = do
  htmlText <- getWebSocketResponse $ GetHtmlForRankingList <$> rankListId

  void $ textArea $ def &
    setValue .~ (fmapMaybe identity htmlText)
