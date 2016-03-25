module Handler.Feed where

import Import

import Model.Feed
import View.Feed

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (race)
import           Control.Lens
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import qualified Network.Wreq              as Wreq
import qualified Text.Atom.Feed            as Atom
import qualified Text.Atom.Feed.Import     as Atom
import           Text.RSS.Import           (elementToRSS)
import           Text.RSS.Syntax           (rssChannel, rssTitle)
import qualified Text.XML.Light            as XML

feedFailure :: Html -> Handler Html
feedFailure msg = setMessage msg >> redirect FeedsR

timeout :: Int
timeout = 10

-- | Time an IO action out after n seconds. Nothing means timeout.
timed :: Int -> IO a -> IO (Maybe a)
timed n = fmap (either (const Nothing) Just) . race (threadDelay $ n * 1000000)

getFeedR :: Handler Html
getFeedR = runMaybeT lookupParams >>= \case
        Nothing -> feedFailure "Missing parameter 'type' or 'url', or unknown 'type'."
        Just (Atom, url) -> fetchAtomFeed url
        Just (RSS2, url) -> fetchRssFeed url
  where
    lookupParams :: MaybeT Handler (FeedType, Text)
    lookupParams = (,) <$> MaybeT lookupTypeParam <*> MaybeT lookupUrlParam

    lookupTypeParam :: Handler (Maybe FeedType)
    lookupTypeParam = maybe Nothing readMay <$> lookupGetParam "type"

    lookupUrlParam :: Handler (Maybe Text)
    lookupUrlParam = lookupGetParam "url"

fetchRssFeed, fetchAtomFeed :: Text -> Handler Html
fetchRssFeed  url = fetchFeed elementToRSS     (T.pack . rssTitle . rssChannel)             (Feed RSS2) (rssFeedWidget url)  url
fetchAtomFeed url = fetchFeed Atom.elementFeed (T.pack . Atom.txtToString . Atom.feedTitle) (Feed Atom) (atomFeedWidget url) url

fetchFeed :: (XML.Element -> Maybe a)
          -> (a -> Text)
          -> (Text -> Text -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Feed)
          -> (a -> Widget)
          -> Text
          -> Handler Html
fetchFeed parse_feed make_title make_feed make_widget url =
    runDB (getFeed url) >>= \case
        Nothing   -> fetchFeedWith Wreq.get Nothing
        Just feed -> fetchFeedWith (Wreq.getWith (makeOpts feed)) (Just feed)
  where
    makeOpts :: Entity Feed -> Wreq.Options
    makeOpts (Entity _ Feed{..}) = Wreq.defaults
        & Wreq.header "If-Modified-Since" .~ [feedLastModified]
        & Wreq.header "If-None-Match"     .~ [feedEtag]

    fetchFeedWith :: (String -> IO (Wreq.Response BSL.ByteString)) -> Maybe (Entity Feed) -> Handler Html
    fetchFeedWith get_request mfeed = catch action handler
      where
        action :: Handler Html
        action = liftIO (timed timeout (get_request $ T.unpack url)) >>= \case
            Nothing -> feedFailure . toHtml $  "Operation timed out after " <> T.pack (show timeout) <> " seconds."
            Just resp -> case XML.parseXMLDoc (resp ^. Wreq.responseBody) >>= parse_feed of
                Nothing  -> feedFailure "Parse failed."
                Just parsed_feed -> do
                    let title         = make_title parsed_feed
                        last_modified = resp ^. Wreq.responseHeader "Last-Modified"
                        etag          = resp ^. Wreq.responseHeader "ETag"
                        contents      = BSL.toStrict $ resp ^. Wreq.responseBody

                    runDB $ case mfeed of
                        Nothing                 -> insert_ (make_feed title url last_modified etag contents)
                        Just (Entity feed_id _) -> updateFeed feed_id title last_modified etag contents

                    defaultLayout $ make_widget parsed_feed

        handler :: HttpException -> Handler Html
        handler (StatusCodeException (Status 304 _) _ _) = do
            let
                -- Safe, because a 304 would only be sent if we set If-Modified-Since,
                -- which means the feed indeed already existed in the database.
                Just (Entity _ feed) = mfeed

                -- Safe, because we only insert valid RSS docs.
                Just parsed_feed = XML.parseXMLDoc (feedContents feed) >>= parse_feed

            defaultLayout $ make_widget parsed_feed
        handler (InvalidUrlException _ _) = feedFailure "Invalid URL."
        handler e = throwIO e

getFeedsR :: Handler Html
getFeedsR = do
    feeds <- runDB getAllFeeds
    defaultLayout $(widgetFile "feeds")
