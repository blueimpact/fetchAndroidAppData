
module GetAppDetails
  (fetchDataJP, getDetails)
  where

import Message
import DB
import Protolude
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match
import Data.List (nub)

import Control.Monad.Haskey
import Control.Monad.Except
import Data.Time

data GetAppDataError
  = CannotFetchTopPage HttpException
  | CannotFetchDetailPage HttpException
  | CannotFindDocId
  | CannotFindAppTitle
  | CannotFindIconLink
  | CannotFindDownloadCount
  | CannotFindDevName
  | CannotFindReviewCount
  | CannotFindReviewValue
  deriving (Show)

type FetchDetailsM = ExceptT GetAppDataError IO

instance MonadHttp FetchDetailsM where
  handleHttpException e = throwError $ CannotFetchDetailPage e

fetchDataJP :: ConcurrentDb Schema -> HaskeyT Schema IO ()
fetchDataJP db = do
  -- pPrintNoColor apps
  details <- lift $ do
    runExceptT $ do
      apps <- getTopApps
      mapM getDetails apps

  curTime <- lift $ getCurrentTime
  case details of
    (Left e) -> lift $ putStrLn $ "Error in FetchData:(" <> show curTime <> ")" <> (show e :: Text)
    (Right d) -> do
      let appList = zip (map Rank [1..]) d
      transact_ $ \schema ->
        insertRankingData JP curTime appList schema
          >>= commit_
  -- pPrintNoColor details


getTopApps :: FetchDetailsM [Text]
getTopApps = do
    let reqFun = req GET
          (https "play.google.com" /: "store" /: "apps" /: "category"
             /: "GAME" /: "collection" /: "topselling_free")
          NoReqBody bsResponse $ "hl" =: ("ja" :: Text)

    src <- catchError reqFun (\(CannotFetchDetailPage e) -> throwError $ CannotFetchTopPage e)
    let tr = parseTags $ responseBody src
    let docIdTags = filter (tagOpenAttrNameLit "div" "data-docid" (\_ -> True))
                    $ filter isTagOpen tr
        docIds = map (fromAttrib "data-docid") docIdTags

        appsList = take 20 $ nub docIds
    return $ map decodeUtf8 appsList

getDetails :: Text -> FetchDetailsM AppDetails
getDetails idName = do
  let reqFun = req GET (https "play.google.com" /: "store" /: "apps" /: "details")
              NoReqBody bsResponse $
              ("id" =: idName) <>
              ("authuser" =: (0 :: Int))

  page <- reqFun
    --catchError reqFun (\e -> throwError $ CannotFetchDetailPage e)
  let tr = catMaybes $ map pruneNodes $ parseTree $ responseBody page

      mapCatAndHead e f l = maybe (Left e) Right v
        where v = headMay $ catMaybes $ map f l

      titleNode = catMaybes $ map (filterFun "div" ("class", "id-app-title")) tr
      title = mapCatAndHead CannotFindAppTitle getInnerText titleNode

      detailsPage = "https://play.google.com/store/apps/details?id=" <> idName

      iconNode = catMaybes $ map (filterFun "div" ("class", "cover-container")) tr
      iconLink = mapCatAndHead CannotFindIconLink getIcon iconNode

      downloadNode = catMaybes $ map (filterFun "div" ("itemprop", "numDownloads")) tr
      downloadCount = mapCatAndHead CannotFindDownloadCount getInnerText downloadNode

      devNameNode = catMaybes $ map (filterFun "span" ("itemprop", "name")) $
        catMaybes $ map (filterFun "a" ("class", "document-subtitle primary")) tr
      devName = mapCatAndHead CannotFindDevName getInnerText devNameNode

      reviewValueNode = catMaybes $ map (filterFun "div" ("itemprop", "aggregateRating")) tr
      reviewValue = mapCatAndHead CannotFindReviewValue getReviewValue reviewValueNode

      reviewCountNode = catMaybes $ map (filterFun "span" ("class", "reviews-num")) tr
      reviewCount = mapCatAndHead CannotFindReviewCount getInnerText reviewCountNode

      appDetail = AppDetails <$> title <*> pure detailsPage
        <*> iconLink <*> devName <*> reviewValue <*> downloadCount <*> reviewCount

  case appDetail of
    (Left e) -> throwError e
    (Right a) -> return a
  -- when (appDetail == Nothing) $ do
  --   curTime <- getCurrentTime
  --   pPrintNoColor curTime
  --   pPrintNoColor titleNode
  --   pPrintNoColor title
  --   pPrintNoColor iconNode
  --   pPrintNoColor iconLink
  --   pPrintNoColor downloadNode
  --   pPrintNoColor downloadCount
  --   pPrintNoColor devNameNode
  --   pPrintNoColor devName
  --   pPrintNoColor reviewValueNode
  --   pPrintNoColor reviewValue
  --   pPrintNoColor reviewCountNode
  --   pPrintNoColor reviewCount

getInnerText (TagBranch _ _ (TagLeaf (TagText t):_)) = Just $ decodeUtf8 t
getInnerText _ = Nothing

-- <div class="cover-container"> <img class="cover-image" src="//lh3.googleusercontent.com/HHtNhRHpPcuB0xDO7FIr-AOvipUW6_ozqG9_7SBFH3C8R2vg_iaqHKiQMOUcs5LAbCKU=w300" alt="Cover art" aria-hidden="true" itemprop="image"> </div>
getIcon (TagBranch _ _ ((TagLeaf (TagOpen _ (_:(_,src):_))):_)) = Just $ "https:" <> (decodeUtf8 src)
getIcon _ = Nothing

-- <a class="document-subtitle primary" href="/store/apps/dev?id=6598096594674427568"> <span itemprop="name">Playrix Games</span> </a>

getNestedText (TagBranch _ _ (tag:_)) = getInnerText tag
getNestedText _ = Nothing

getReviewValue (TagBranch _ _ (_:_:tag:_)) = getInnerText tag
getReviewValue _ = Nothing

filterFun :: _ -> _ -> _ -> Maybe _
filterFun _ _ (TagLeaf _) = Nothing
filterFun div attr t@(TagBranch d a tags) =
  if (d == div) && (elem attr a)
    then Just t
    else headMay $ catMaybes $ map (filterFun div attr) tags

-- Remove blank text nodes
pruneNodes (TagBranch d a tags) = Just $ TagBranch d a (catMaybes $ map pruneNodes tags)
pruneNodes (TagLeaf t) = if t == (TagText " ") then Nothing else Just $ TagLeaf t
