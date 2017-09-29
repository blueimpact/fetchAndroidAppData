{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Protolude hiding (div)
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match

import Text.Pretty.Simple (pPrintNoColor, pShow)
-- import Control.Exception (throwIO)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html.Renderer.Text
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Network.Wai
import Network.HTTP.Types.Header

import GHC.Generics
import Data.Aeson
import Data.List (nub)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock

instance MonadHttp IO where
  handleHttpException = throwIO


main :: IO ()
main = do
  let app req respond = do
        bs <- makeDetailsPage
        let header = [(hContentType, "text/plain"), ("charset", "utf-8")
                     , ("content-disposition", "attachement; filename = \"androidAppData.txt\"")]
        respond $ responseLBS ok200 header bs
  runEnv 8080 app


-- makeDetailsPage :: IO ByteString
makeDetailsPage = do
  apps <- getTopApps
  -- pPrintNoColor apps
  details <- mapM getDetails apps
  topTableDetails <- getDetails "com.blueimpact.mof"
  -- pPrintNoColor details

  curTime <- getCurrentTime

  let doc = mconcat $ header : header2 : topDoc :
              (map getTable $ zip [1..] $ catMaybes details)
      topDoc = maybe (div $ text "") identity $ (getTopTable <$> topTableDetails)
      docString = Text.Blaze.Html.Renderer.Text.renderHtml doc
      header = h3 $ text "Androidトップ無料ゲームランキング【1〜20位まで一挙公開！】"
      header2 = preEscapedToHtml $ "[aside type=\"normal\"]\
          \<strong>ランキング調査方法</strong><br>\
          \参考：GooglePlayトップ無料ゲームTOP20<br>"   <> date <> "[/aside]"

      (y,m,d) = toGregorian $ utctDay curTime
      date = show y <> "年" <> show m <> "月" <> show d <> "日調べ" :: Text

  return $ Data.Text.Lazy.Encoding.encodeUtf8 $ docString

getTable (rank,(AppDetails appN appD appI appDev appRev appDC appRC)) = do
  table $ tbody $ do
    tr $ td ! class_ "granktitle" ! colspan "2" $ do
      img ! src (toValue $ rankLink rank) ! alt (toValue $ rankAltVal rank)
        ! width "40" ! height "40"
      a ! href (toValue appD) ! (customAttribute "target" "_blank") $ text appN
    tr $ do
      td ! class_ "grank" $ do
        a ! href (toValue appD) ! (customAttribute "target" "_blank") $ do
          img ! class_ "aligncenter" !
            src (toValue appI) ! alt "g-rank" ! width "100" ! height "100"
      td ! class_ "grank" $ do
        ul ! class_ "grank" $ do
          li $ text $ "開発者：" <> appDev
          li $ text $ "レビュー評価：" <> appRev
          li $ text $ "レビュー数：" <> appRC <> "件"
          li $ text $ "ダウンロード数：" <> appDC
          li $ text $ "前回順位：" <> show rank <> "位"


getTopTable (AppDetails appN appD appI appDev appRev appDC appRC) = do
  table $ tbody $ do
    tr $ td ! class_ "granktitle" ! colspan "2" $ do
      img ! src "http://www.app510.net/int/wp-content/uploads/rank-pr.png"
        ! alt "rank-pr"
        ! width "40" ! height "40"
      a ! href (toValue appD) ! (customAttribute "target" "_blank") $ text appN
    tr $ do
      td ! class_ "grank" $ do
        a ! href (toValue appD) ! (customAttribute "target" "_blank") $ do
          img ! class_ "aligncenter" !
            src (toValue appI) ! alt "g-rank" ! width "100" ! height "100"
      td ! class_ "grank" $ do
        ul ! class_ "grank" $ do
          li $ text $ "開発者：" <> appDev
          li $ text $ "レビュー評価：" <> appRev
          li $ text $ "レビュー数：" <> appRC <> "件"
          li $ text $ "ダウンロード数：" <> appDC

rankLink :: Int -> Text
rankLink r = "http://www.app510.net/int/wp-content/uploads/rank-i" <> val <> ".png"
  where val = if r < 10
                 then "0" <> show r
                 else show r

rankAltVal :: Int -> Text
rankAltVal r = "rank-i" <> val
  where val = if r < 10
                 then "0" <> show r
                 else show r

-- Find Details Code
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
  deriving (Show)

getTopApps :: IO [Text]
getTopApps = do
    src <- req GET
      (https "play.google.com" /: "store" /: "apps" /: "category"
         /: "GAME" /: "collection" /: "topselling_free")
      NoReqBody bsResponse $ "hl" =: ("ja" :: Text)
    let tr = parseTags $ responseBody src
    let docIdTags = filter (tagOpenAttrNameLit "div" "data-docid" (\_ -> True))
                    $ filter isTagOpen tr
        docIds = map (fromAttrib "data-docid") docIdTags

        appsList = take 20 $ nub docIds
    return $ map decodeUtf8 appsList

-- getDetails :: Text -> IO (Maybe AppDetails)
getDetails idName = do
  page <- req GET (https "play.google.com" /: "store" /: "apps" /: "details")
              NoReqBody bsResponse $
              ("id" =: idName) <>
              ("authuser" =: (0 :: Int))

  let tr = catMaybes $ map pruneNodes $ parseTree $ responseBody page
      titleNode = catMaybes $ map (filterFun "div" ("class", "id-app-title")) tr

      title = headMay $ map getInnerText titleNode

      detailsPage = "https://play.google.com/store/apps/details?id=" <> idName

      iconNode = catMaybes $ map (filterFun "div" ("class", "cover-container")) tr
      iconLink = headMay $ map getIcon iconNode

      downloadNode = catMaybes $ map (filterFun "div" ("itemprop", "numDownloads")) tr
      downloadCount = headMay $ map getInnerText downloadNode

      devNameNode = catMaybes $ map (filterFun "span" ("itemprop", "name")) $
        catMaybes $ map (filterFun "a" ("class", "document-subtitle primary")) tr
      devName = headMay $ map getInnerText devNameNode

      reviewValueNode = catMaybes $ map (filterFun "div" ("itemprop", "aggregateRating")) tr
      reviewValue = headMay $ map getReviewValue reviewValueNode

      reviewCountNode = catMaybes $ map (filterFun "span" ("class", "reviews-num")) tr
      reviewCount = headMay $ map getInnerText reviewCountNode

      appDetail = AppDetails <$> title <*> pure detailsPage
        <*> iconLink <*> devName <*> reviewValue <*> downloadCount <*> reviewCount

  -- -- pPrintNoColor titleNode
  -- pPrintNoColor title
  -- -- pPrintNoColor iconNode
  -- pPrintNoColor iconLink
  -- -- pPrintNoColor downloadNode
  -- pPrintNoColor downloadCount
  -- -- pPrintNoColor devNameNode
  -- pPrintNoColor devName
  -- -- pPrintNoColor reviewValueNode
  -- pPrintNoColor reviewValue
  -- -- pPrintNoColor reviewCountNode
  -- pPrintNoColor reviewCount

  return appDetail

getInnerText (TagBranch _ _ (TagLeaf (TagText t):_)) = decodeUtf8 t

-- <div class="cover-container"> <img class="cover-image" src="//lh3.googleusercontent.com/HHtNhRHpPcuB0xDO7FIr-AOvipUW6_ozqG9_7SBFH3C8R2vg_iaqHKiQMOUcs5LAbCKU=w300" alt="Cover art" aria-hidden="true" itemprop="image"> </div>
getIcon (TagBranch _ _ ((TagLeaf (TagOpen _ (_:(_,src):_))):_)) = "https:" <> (decodeUtf8 src)

-- <a class="document-subtitle primary" href="/store/apps/dev?id=6598096594674427568"> <span itemprop="name">Playrix Games</span> </a>

getNestedText (TagBranch _ _ (tag:_)) = getInnerText tag
getReviewValue (TagBranch _ _ (_:_:tag:_)) = getInnerText tag

filterFun :: _ -> _ -> _ -> Maybe _
filterFun _ _ (TagLeaf _) = Nothing
filterFun div attr t@(TagBranch d a tags) =
  if (d == div) && (elem attr a)
    then Just t
    else headMay $ catMaybes $ map (filterFun div attr) tags

-- Remove blank text nodes
pruneNodes (TagBranch d a tags) = Just $ TagBranch d a (catMaybes $ map pruneNodes tags)
pruneNodes (TagLeaf t) = if t == (TagText " ") then Nothing else Just $ TagLeaf t
