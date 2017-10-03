{-# LANGUAGE DeriveAnyClass #-}
module Message where

import Protolude
import Data.Aeson
import Data.Text
import Data.Time

import Reflex.Dom.WebSocket.Message

type AppRequest =
  GetListOfRankings
  :<|> GetDetailsForRankingList
  :<|> GetHtmlForRankingList

-- Messages
data GetListOfRankings = GetListOfRankings
  deriving (Show, Generic, FromJSON, ToJSON)

instance WebSocketMessage AppRequest GetListOfRankings where
  type ResponseT AppRequest GetListOfRankings = [RankingListId]


data GetDetailsForRankingList = GetDetailsForRankingList RankingListId
  deriving (Show, Generic, FromJSON, ToJSON)

instance WebSocketMessage AppRequest GetDetailsForRankingList where
  type ResponseT AppRequest GetDetailsForRankingList
    = [(Rank, AppDetails)]


data GetHtmlForRankingList = GetHtmlForRankingList RankingListId
  deriving (Show, Generic, FromJSON, ToJSON)

instance WebSocketMessage AppRequest GetHtmlForRankingList where
  type ResponseT AppRequest GetHtmlForRankingList = Maybe Text

-- Data Types
newtype RankingListId = RankingListId UTCTime
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

newtype Rank = Rank Int
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data AppDetails = AppDetails
  {
    appName :: Text
    , appDetailPage :: Text
    , appIconLink :: Text
    , appDeveloper :: Text
    , appReviewValue :: Text
    , appDownloadCount :: Text
    , appReviewCount :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
