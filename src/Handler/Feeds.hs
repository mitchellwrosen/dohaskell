module Handler.Feeds where

import Import

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Lens             ((^.))
import           Data.ByteString.Lazy     (ByteString)
import           Data.Map                 ((!))
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Network.Wreq             as Wreq
import           Text.RSS.Import          (elementToRSS)
import           Text.RSS.Syntax          (RSS(..), RSSChannel(..), RSSItem(..))
import           Text.XML.Light           (parseXMLDoc)

getFeedR :: Handler Html
getFeedR = defaultLayout $
    lookupGetParam "url" >>= \case
        Nothing -> [whamlet|Missing param 'url'|]
        Just url -> do
            liftIO (fetchFeed url) >>= \case
                Left _ -> [whamlet|Operation timed out after #{show timeout} seconds.|]
                Right resp ->
                    maybe
                      ([whamlet|RSS parse failed.|])
                      (rssFeedWidget url)
                      (parseXMLDoc (resp ^. Wreq.responseBody) >>= elementToRSS)
  where
    fetchFeed :: Text -> IO (Either () (Wreq.Response ByteString))
    fetchFeed = race (threadDelay $ timeout * 1000000) . Wreq.get . T.unpack

    timeout :: Int
    timeout = 30 -- seconds

rssFeedWidget :: Text -> RSS -> Widget
rssFeedWidget url RSS{..} = do
    resources <- handlerToWidget . runDB $ selectList [] []
    let existing_urls  = S.fromList . map (resourceUrl . entityVal) $ resources
        url_to_res_map = foldr (uncurry M.insert . ((resourceUrl . entityVal) &&& id)) mempty resources

    [whamlet|
      <div .header>#{rssTitle rssChannel} (#{url})
      <div .items>
        $forall item <- rssItems rssChannel
          $maybe title <- rssItemTitle item
            $maybe link <- T.pack <$> rssItemLink item
              <div .item>
                $if S.member link existing_urls
                  $with Entity res_id _ <- (!) url_to_res_map link
                    <a .exists href=@{ResourceR res_id}>#{title}
                $else
                  <a .not-exists href=#{link}>#{title}
    |]
    toWidget [cassius|
      .header
        border-bottom: 1px solid black
        font-weight: bold

      .exists
        color: gray

      .not-exists
        font-weight: bold

      a
        color: #069
    |]
