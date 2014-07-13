module View.Browse
    ( WhichLink(..)
    , authorListWidget
    , browseBarWidget
    , resourceListWidget
    , tagListWidget
    ) where

import Import

import Model.Resource
import Model.User

import           Data.Aeson.Types       (Value(..))
import qualified Data.Map               as M
import           Data.Map               ((!))
import qualified Data.Set               as S
import qualified Data.Text              as T

-- TODO: There's got to be a smarter way to do this.
data WhichLink = AuthorsLink | ResourcesLink | TagsLink deriving Eq

fontWeight :: WhichLink -> WhichLink -> Text
fontWeight x y | x == y = "bold"
fontWeight _ _          = "normal"

browseBarWidget :: WhichLink -> Widget
browseBarWidget bold = do
  [whamlet|
    <div .browse-bar>browse by: #
      <a .browse-link #br-auth href=@{BrowseAuthorsR}>author
      <a .browse-link #br-tags href=@{BrowseTagsR}>tag
      |
      <a .browse-link #br-res href=@{BrowseResourcesR}>list all
  |]
  toWidget [cassius|
    .browse-bar
      border-bottom: 1px solid black
      font-size: 1.1em
      font-variant: small-caps
      height: 1.1em
      line-height: 1.1em
      margin-bottom: 4px

    .browse-link
      color: #069

    a.browse-link:hover
      text-decoration: none

    #br-auth
      font-weight: #{fontWeight bold AuthorsLink}

    #br-res
      font-weight: #{fontWeight bold ResourcesLink}

    #br-tags
      font-weight: #{fontWeight bold TagsLink}
  |]

resourceListWidget :: [Entity Resource] -> Widget
resourceListWidget resources = do
    let resource_ids = map entityKey resources

    authorsMap <- handlerToWidget . runDB $ getAuthorsIn resource_ids

    (is_logged_in, favs, grokked) <- handlerToWidget $
        maybeAuthId >>= \case
            Nothing  -> return (False, mempty, mempty)
            Just uid -> runDB $ (,,)
                <$> pure True
                <*> getFavoriteResourcesIn resource_ids uid
                <*> getGrokkedResourcesIn  resource_ids uid

    $(widgetFile "resource-list")

tagListWidget :: [Entity Tag] -> Map TagId Int -> Maybe (Map TagId Int) -> Widget
tagListWidget = fieldListWidget TagR (String "/tag/") tagTag

authorListWidget :: [Entity Author] -> Map AuthorId Int -> Maybe (Map AuthorId Int) -> Widget
authorListWidget = fieldListWidget AuthorR (String "/author/") authorName

fieldListWidget :: (Text -> Route App)  -- makes a full route to /tag/#Text or /author/#Text
                -> Value                -- "/tag/" or "/author/", for AJAX - so don't change the route!
                -> (val -> Text)        -- gets the Text from the Tag/Author (tag, or name)
                -> [Entity val]         -- Tags or Authors
                -> Map (Key val) Int
                -> Maybe (Map (Key val) Int)
                -> Widget
fieldListWidget route path_piece text_func fields total_counts mgrokked_counts = $(widgetFile "tag-or-author-list")
