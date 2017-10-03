
module GetAppDetails where

import Message
import Protolude
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Match
import Data.List (nub)
instance MonadHttp IO where
  handleHttpException = throwIO

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
