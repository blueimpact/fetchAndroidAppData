module Widgets where

import Protolude hiding (link, (&))
import Reflex.Dom
import Message

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import qualified Data.Map as Map
import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import Data.Time

import Web.Reflex.Bootstrap.Pagination
import Web.Reflex.Bootstrap.Table
import Web.Reflex.Bootstrap.Markup

type AppMonadT t m = WithWebSocketT Message.AppRequest t m
type AppMonad t m = (MonadWidget t m)

topWidget
  :: AppMonad t m => AppMonadT t m ()
topWidget = do
  ev <- getPostBuild
  rankingLists <- getWebSocketResponse $ GetListOfRankings 1 <$ ev

  rec
    let
      latestRankingEv = fmapMaybe headMay $ fst <$> rankingLists
      htmlEv = leftmost [htmlReqEv,latestRankingEv]

      htmlReqEv = switchPromptlyDyn $ map leftmost listDyn

    appHtmlPage htmlEv

    listDyn <- renderList (Just 5) (identity) renderListItem
      (\x -> do
          res <- getWebSocketResponse $ GetListOfRankings <$> x
          return $ (uncurry PagedList) <$> res)

  return ()

renderListItem
  :: AppMonad t m
  => RankingListId
  -> AppMonadT t m (Event t RankingListId)
  -- Clicked on "Get Html"
renderListItem rankListId = do
  tableHover (Map.empty) $ do
    (d,h) <- tr $ do
      td $ renderRankinkListId rankListId
      detailsEv <- td $ primaryButton "Details"
      htmlEv <- td $ primaryButton "Get Html"
      return (detailsEv, htmlEv)

    detailsEv <- getWebSocketResponse
      $ GetDetailsForRankingList rankListId <$ d

    widgetHold (return ())
      (fetchDetailsAndRender <$> (fmapMaybe identity detailsEv))

    return $ rankListId <$ h

renderRankinkListId (RankingListId t) = do
  text $ show $ utctDay t

fetchDetailsAndRender
  :: AppMonad t m
  => [(Rank, AppDetails)]
  -> AppMonadT t m ()
fetchDetailsAndRender ls = do
  elAttr "div" ("style" =: "overflow: auto; height: 400px;") $ do
    mapM_ renderAppDetails ls

renderAppDetails
  :: AppMonad t m
  => (Rank, AppDetails)
  -> AppMonadT t m ()
renderAppDetails (rank, d) = do
  elClass "div" "" $ text $ show (rank)
  tableHover (Map.empty) $ do
    tr $ do
      td $ text "Name"
      td $ text $ appName d
    tr $ do
      td $ text "Developer"
      td $ text $ appDeveloper d
    tr $ do
      td $ text "Review Value"
      td $ text $ appReviewValue d
    tr $ do
      td $ text "Review Count"
      td $ text $ appReviewCount d
    tr $ do
      td $ text "Downloads"
      td $ text $ appDownloadCount d
    tr $ do
      td $ text "Icon Link"
      td $ text $ appIconLink d
    tr $ do
      td $ text "Details Page"
      td $ text $ appDetailPage d

appHtmlPage
  :: AppMonad t m
  => Event t RankingListId
  -> AppMonadT t m ()
appHtmlPage rankListId = do
  htmlText <- getWebSocketResponse $ GetHtmlForRankingList <$> rankListId

  let showDate d = elAttr "h1" ("style" =: "text-align: center;") d

  widgetHold (return()) $ showDate <$> (renderRankinkListId <$> rankListId)
  void $ textArea $ def &
    setValue .~ (fmapMaybe identity htmlText) &
    textAreaConfig_attributes .~ (constDyn $ "style" =: "height: 800px; width: 1200px;")
