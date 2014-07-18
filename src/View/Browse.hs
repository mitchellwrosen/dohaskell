module View.Browse
    ( BrowseByLink(..)
    , authorListWidget
    , browseBarWidget
    , resourceListWidget
    , tagListWidget
    , typeListWidget
    ) where

import Import

import Model.Resource
import Model.User

import           Data.Aeson.Types       (Value(..))
import           Data.Function          (on)
import           Data.List              (sortBy)
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T

-- TODO: There's got to be a smarter way to do this.
data BrowseByLink
    = BrowseByAuthorLink
    | BrowseByResourceLink
    | BrowseByTagLink
    | BrowseByTypeLink
    deriving Eq

fontWeight :: BrowseByLink -> BrowseByLink -> Text
fontWeight x y | x == y = "bold"
fontWeight _ _          = "normal"

browseBarWidget :: BrowseByLink -> Widget
browseBarWidget embolden = do
  [whamlet|
    <div .browse-bar>browse by: #
      <a .browse-link #br-auth href=@{BrowseAuthorsR}>author
      <a .browse-link #br-tag href=@{BrowseTagsR}>tag
      <a .browse-link #br-type href=@{BrowseTypesR}>type
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
      font-weight: #{fontWeight embolden BrowseByAuthorLink}

    #br-res
      font-weight: #{fontWeight embolden BrowseByResourceLink}

    #br-tag
      font-weight: #{fontWeight embolden BrowseByTagLink}

    #br-type
      font-weight: #{fontWeight embolden BrowseByTypeLink}
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
tagListWidget tags total_counts mgrokked_counts =
    fieldListWidget
      TagR
      (String "/tag/")
      tagTag
      (map (entityKey &&& entityVal) tags)
      total_counts
      mgrokked_counts

authorListWidget :: [Entity Author] -> Map AuthorId Int -> Maybe (Map AuthorId Int) -> Widget
authorListWidget authors total_counts mgrokked_counts =
    fieldListWidget
      AuthorR
      (String "/author/")
      authorName
      (map (entityKey &&& entityVal) authors)
      total_counts
      mgrokked_counts

typeListWidget :: Map ResourceType Int -> Maybe (Map ResourceType Int) -> Widget
typeListWidget =
    fieldListWidget
      TypeR
      (String "/type/")
      shortDescResourceTypePlural
      (zip res_types res_types)
  where
    res_types = sortBy (compare `on` shortDescResourceTypePlural) [minBound..maxBound]

fieldListWidget :: Ord key
                => (Text -> Route App)  -- makes a full route to /tag/#Text, /author/#Text, or /type/#Text
                -> Value                -- "/tag/", "/author/", "/type/" for AJAX - so don't change the route!
                -> (val -> Text)        -- gets the text to display in each row (and also construct the route with)
                -> [(key, val)]         -- key/val assoc list
                -> Map key Int          -- total counts map
                -> Maybe (Map key Int)  -- grokked counts map (Nothing if not logged in)
                -> Widget
fieldListWidget route path_piece text_func fields total_counts mgrokked_counts = $(widgetFile "browse-list")
