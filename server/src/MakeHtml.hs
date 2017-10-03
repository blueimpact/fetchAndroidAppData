module MakeHtml where

import Message
import Protolude hiding (div)
import Text.Pretty.Simple (pPrintNoColor, pShow)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html.Renderer.Text
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding
import Data.Text (Text)

import GHC.Generics
import Data.Aeson
import Data.List (nub)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock

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
