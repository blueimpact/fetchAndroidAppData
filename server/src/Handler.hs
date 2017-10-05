
module Handler where

import Message
import GetAppDetails
import MakeHtml
import DB

import Protolude hiding (div)
import Data.Time

import Control.Monad.Haskey

type HandlerM = HaskeyT Schema IO

getListOfRankings :: GetListOfRankings
  -> HandlerM ([RankingListId], Word)
getListOfRankings (GetListOfRankings page) =
  transactReadOnly
    (\x -> do
        n <- queryAllRankingsLists JP x
        c <- queryPagedRankingsLists JP (fromInteger (toInteger (page - 1))  * 5, 5) x
        let pages = ceiling $ (fromIntegral (length n)) / 5
        return (map fst c, pages)
    )

getDetailsForRankingList :: GetDetailsForRankingList
  -> HandlerM (Maybe RankingList)
getDetailsForRankingList (GetDetailsForRankingList r) =
  transactReadOnly (queryList JP r)

getHtmlForRankingList :: GetHtmlForRankingList
  -> HandlerM (Maybe Text)
getHtmlForRankingList (GetHtmlForRankingList r@(RankingListId utcTime)) = do
  l <- transactReadOnly (queryList JP r)
  topTableDetails <- liftIO $ runExceptT $ do
    getDetails "com.blueimpact.mof"
  let h = makeDetailsPage <$> l <*> pure utcTime
            <*> (either (const Nothing) Just topTableDetails)
  return h
