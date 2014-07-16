module View.Feed where

import Import

import           Data.Map        ((!))
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Text.Atom.Feed  as Atom
import           Text.RSS.Syntax (RSS(..), RSSChannel(..), RSSItem(..))

atomFeedWidget :: Text -> Atom.Feed -> Widget
atomFeedWidget url atom_feed = do
    undefined

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
