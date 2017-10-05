
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
  ls <- transactReadOnly (queryAllRankingsLists JP)
  let prevDay = join $ headMay
        <$> (tailMay $ dropWhile (> r) (reverse $ map fst ls))
  p <- mapM (\pr -> transactReadOnly (queryList JP pr)) prevDay
  topTableDetails <- liftIO $ runExceptT $ do
    getDetails "com.blueimpact.mof"
  let h = makeDetailsPage <$> details <*> pure utcTime
            <*> (either (const Nothing) Just topTableDetails)
      details = map (getPrevDayRankings $ join p) <$> l
  return h

getPrevDayRankings
  :: Maybe RankingList
  -> (Rank, AppDetails)
  -> (Maybe Rank, Rank, AppDetails)
getPrevDayRankings ps (r,app) = (fst <$> p,r,app)
  where p = join $ headMay <$>
          (filter (\(_,x) -> appName x == appName app) <$> ps)
