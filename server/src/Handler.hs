
module Handler where


import GetAppDetails
import MakeHtml
import DB

import Protolude hiding (div)

import Control.Monad.Haskey

type HandlerM = HaskeyT Schema IO

getListOfRankings :: GetListOfRankings
  -> HandlerM [RankingListId]
getListOfRankings _ = return []

getDetailsForRankingList :: GetDetailsForRankingList
  -> HandlerM [(Rank, AppDetails)]
getDetailsForRankingList _ = return []

getHtmlForRankingList :: GetHtmlForRankingList
  -> HandlerM (Maybe Text)
getHtmlForRankingList _ = return Nothing
