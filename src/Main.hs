{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match

import Text.Pretty.Simple (pPrintNoColor)
-- import Control.Exception (throwIO)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)
import qualified Data.Text as T
import Data.Text (Text)
import Network.Wai.Handler.Warp
import GHC.Generics
import Data.Aeson
import Data.List (nub)
import Data.Text.Encoding
instance MonadHttp IO where
  handleHttpException = throwIO


main :: IO ()
main = do
  apps <- getTopApps
  pPrintNoColor apps
  details <- mapM getDetails apps
  pPrintNoColor details
  return ()


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

  let tr = parseTree $ responseBody page
      titleNode = concat $ map (filterFun "div" ("class", "id-app-title")) tr

      title = headMay $ map getInnerText titleNode

      detailsPage = "https://play.google.com/store/apps/details?id=" <> idName

      iconNode = concat $ map (filterFun "img" ("class", "cover-image")) tr
      iconLink = headMay $ map getIcon iconNode

      downloadNode = concat $ map (filterFun "div" ("itemprop", "numDownloads")) tr
      downloadCount = headMay $ map getInnerText downloadNode

      devNameNode = concat $ map (filterFun "a" ("class", "document-subtitle primary")) tr
      devName = headMay $ map getNextedText devNameNode

      reviewValueNode = concat $ map (filterFun "div" ("itemprop", "aggregateRating")) tr
      reviewValue = headMay $ map getNextedText reviewValueNode

      reviewCountNode = concat $ map (filterFun "span" ("class", "reviews-num")) tr
      reviewCount = headMay $ map getInnerText reviewCountNode

      appDetail = AppDetails <$> title <*> pure detailsPage
        <*> iconLink <*> devName <*> reviewValue <*> downloadCount <*> reviewCount

  pPrintNoColor titleNode
  pPrintNoColor title
  pPrintNoColor iconNode
  pPrintNoColor iconLink
  pPrintNoColor downloadNode
  pPrintNoColor downloadCount
  pPrintNoColor devNameNode
  pPrintNoColor devName
  pPrintNoColor reviewValueNode
  pPrintNoColor reviewValue
  pPrintNoColor reviewCountNode
  pPrintNoColor reviewCount
  
  return appDetail

getInnerText (TagBranch _ _ [TagLeaf (TagText t)]) = decodeUtf8 t
-- <img class="cover-image" src="//lh3.googleusercontent.com/HHtNhRHpPcuB0xDO7FIr-AOvipUW6_ozqG9_7SBFH3C8R2vg_iaqHKiQMOUcs5LAbCKU=w300" alt="Cover art" aria-hidden="true" itemprop="image">
getIcon (TagLeaf (TagOpen _ (_:(_,src):_))) = "https:" <> (decodeUtf8 src)

-- <a class="document-subtitle primary" href="/store/apps/dev?id=6598096594674427568"> <span itemprop="name">Playrix Games</span> </a>

getNextedText (TagBranch _ _ [tag]) = getInnerText tag

filterFun _ _ (TagLeaf _) = []
filterFun div attr t@(TagBranch d a tags) =
  if (d == div) && (elem attr a)
    then [t]
    else concat $ map (filterFun div attr) tags
