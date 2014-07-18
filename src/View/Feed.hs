module View.Feed
    ( atomFeedWidget
    , rssFeedWidget
    ) where

import Import

import           Database.Esqueleto
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Text.Atom.Feed     (Entry(..), Link(..), feedEntries, txtToString)
import qualified Text.Atom.Feed     as Atom
import           Text.Blaze         (ToMarkup)
import           Text.RSS.Syntax    (RSS(..), RSSChannel(..), RSSItem(..))

makeUrlToResIdMap :: YesodDB App (Map Text ResourceId)
makeUrlToResIdMap = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value Text, Value ResourceId)]
    sel = select $
          from $ \r ->
          return (r^.ResourceUrl, r^.ResourceId)

-- | Get an Entry's rel="alternate" link (no rel implies rel="alternate").
-- There can be multiple alternate links, each with a different
-- type/hreflang combination, but we're just being dumb and grabbing
-- the first one.
getEntryAlternateLink :: Atom.Entry -> Atom.Link
getEntryAlternateLink = go . entryLinks
  where
    go (x@(Atom.Link _ Nothing _ _ _ _ _ _):_) = x
    go (x@(Atom.Link _ (Just (Right "alternate")) _ _ _ _ _ _):_) = x
    go (_:xs) = go xs
    go [] = error "RFC 4287 violation, rel=alternate link missing"

atomFeedWidget :: Text -> Atom.Feed -> Widget
atomFeedWidget feed_url Atom.Feed{..} = do
    url_to_res_id_map <- handlerToWidget $ runDB makeUrlToResIdMap
    feedHeader (txtToString feedTitle) feed_url
    [whamlet|
      <div .entries>
        $forall entry <- feedEntries
          $with link_uri <- T.pack $ linkHref (getEntryAlternateLink entry)
            <div .entry>
              $maybe res_id <- M.lookup link_uri url_to_res_id_map
                <a .exists href=@{ResourceR res_id}>#{txtToString $ entryTitle entry}
              $nothing
                <a .not-exists href=#{link_uri}>#{txtToString $ entryTitle entry}
    |]
    feedCassius

rssFeedWidget :: Text -> RSS -> Widget
rssFeedWidget feed_url RSS{..} = do
    url_to_res_id_map <- handlerToWidget $ runDB makeUrlToResIdMap
    feedHeader (rssTitle rssChannel) feed_url
    [whamlet|
      <div .entries>
        $forall item <- rssItems rssChannel
          $with title <- fromMaybe "(no title)" (rssItemTitle item)
            $maybe link <- T.pack <$> rssItemLink item
              <div .entry>
                $maybe res_id <- M.lookup link url_to_res_id_map
                  <a .exists href=@{ResourceR res_id}>#{title}
                $nothing
                  <a .not-exists href=#{link}>#{title}
    |]
    feedCassius

feedHeader :: (ToMarkup a, ToMarkup b) => a -> b -> Widget
feedHeader title url = do
    [whamlet|
      <div .header>
        <a href=@{FeedsR}>Back
        <div>#{title} (#{url})
    |]
    toWidget [cassius|
      .header a
        display: inline-block
        margin-bottom: 5px

      .header
        border-bottom: 1px solid black
        font-weight: bold
        margin-bottom: 5px
    |]

feedCassius :: Widget
feedCassius =
    toWidget [cassius|
      .exists
        color: gray

      .not-exists
        font-weight: bold

      .entry
        font-size: 1.2em
        line-height: 1.3em
        padding-left: 5px

      .entry a
        display: block
        padding-left: 5px
        text-decoration: none

      .entry a:hover
        background-color: #eee

      .entry a.exists:hover
        color: gray

      .entry a.not-exists:hover
        color: black

    |]
