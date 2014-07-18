module Model.Feed
    ( getAllFeeds
    , getFeed
    , updateFeed
    , module Model.Feed.Internal
    ) where

import Import

import Model.Feed.Internal

import Data.ByteString (ByteString)
import Database.Esqueleto

-- | Get all feed (title, url, type) 3-tuples.
getAllFeeds :: YesodDB App [(Text, Text, FeedType)]
getAllFeeds = fmap (map fromValue) $
    select $
    from $ \f -> do
    orderBy [asc (f^.FeedUrl)]
    return (f^.FeedTitle, f^.FeedUrl, f^.FeedType)

getFeed :: Text -> YesodDB App (Maybe (Entity Feed))
getFeed = getBy . UniqueFeed

updateFeed :: FeedId -> Text -> ByteString -> ByteString -> ByteString -> YesodDB App ()
updateFeed feed_id title last_modified etag contents =
    update $ \f -> do
    set f [ FeedTitle        =. val title
          , FeedLastModified =. val last_modified
          , FeedEtag         =. val etag
          , FeedContents     =. val contents
          ]
    where_ (f^.FeedId ==. val feed_id)
