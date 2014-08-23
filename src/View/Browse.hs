module View.Browse
    ( BrowseByLink(..)
    , browseBarWidget
    , resourceListWidget
    , sortAuthorBarWidget
    , sortTagBarWidget
    , typeListWidget
    ) where

import Import

import Handler.Utils
import Model.Browse
import Model.Resource
import Model.User

import           Data.Aeson.Types       (Value(..))
import           Data.Function          (on)
import           Data.List              (sortBy)
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T

boldIfEq :: Eq a => a -> a -> Text
boldIfEq x y | x == y = "bold"
boldIfEq _ _          = "normal"

browseBarWidget :: BrowseByLink -> Widget
browseBarWidget browse_by_link = do
    [whamlet|
      <div .browse-bar>browse by: #
        <a .browse-link #br-auth href=@{BrowseAuthorsR}>author
        |
        <a .browse-link #br-tag href=@{BrowseTagsR}>tag
        |
        <a .browse-link #br-type href=@{BrowseTypesR}>type
        |
        <a .browse-link #br-res href=@{BrowseResourcesR}>list all
    |]
    toWidget [cassius|
      .browse-bar
        font-size: 1.1em
        font-variant: small-caps
        height: 1.1em
        line-height: 1.1em

      .browse-link
        color: #069

      a.browse-link:hover
        text-decoration: none

      #br-auth
        font-weight: #{boldIfEq browse_by_link BrowseByAuthorLink}

      #br-res
        font-weight: #{boldIfEq browse_by_link BrowseByResourceLink}

      #br-tag
        font-weight: #{boldIfEq browse_by_link BrowseByTagLink}

      #br-type
        font-weight: #{boldIfEq browse_by_link BrowseByTypeLink}
    |]

sortAuthorBarWidget :: SortBy -> Widget
sortAuthorBarWidget sort_by = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort authors by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-up href=@?{(route, [("sort", "paucity")])}>paucity
        <a .sort-link #so-count-down href=@?{(route, [("sort", "prolificity")])}>prolificity
        |
        <a .sort-link #so-earliest href=@?{(route, [("sort", "senescence")])}>senescence
        <a .sort-link #so-latest href=@?{(route, [("sort", "pubescence")])}>pubescence
    |]
    sortBarCSS
    toWidget [cassius|
      #so-az
        font-weight: #{boldIfEq sort_by SortByAZ}

      #so-count-up
        font-weight: #{boldIfEq sort_by SortByCountUp}

      #so-count-down
        font-weight: #{boldIfEq sort_by SortByCountDown}

      #so-earliest
        font-weight: #{boldIfEq sort_by SortByEarliest}

      #so-latest
        font-weight: #{boldIfEq sort_by SortByLatest}
    |]

sortTagBarWidget :: SortBy -> Widget
sortTagBarWidget sort_by = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort tags by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-up href=@?{(route, [("sort", "obscurity")])}>obscurity
        <a .sort-link #so-count-down href=@?{(route, [("sort", "profusion")])}>profusion
        |
        <a .sort-link #so-earliest href=@?{(route, [("sort", "senescence")])}>senescence
        <a .sort-link #so-latest href=@?{(route, [("sort", "pubescence")])}>pubescence
    |]
    sortBarCSS
    toWidget [cassius|
      #so-az
        font-weight: #{boldIfEq sort_by SortByAZ}

      #so-count-up
        font-weight: #{boldIfEq sort_by SortByCountUp}

      #so-count-down
        font-weight: #{boldIfEq sort_by SortByCountDown}

      #so-earliest
        font-weight: #{boldIfEq sort_by SortByEarliest}

      #so-latest
        font-weight: #{boldIfEq sort_by SortByLatest}
    |]

-- | CSS that applies to all sort bars.
sortBarCSS :: Widget
sortBarCSS = toWidget
    [cassius|
      .sort-bar
        border-bottom: 1px solid black
        font-size: 1.1em
        font-variant: small-caps
        height: 1.1em
        line-height: 1.1em
        margin-bottom: 4px

      .sort-link
        color: #069

      a.sort-link:hover
        text-decoration: none


    |]

resourceListWidget :: [Entity Resource] -> Widget
resourceListWidget resources = do
    let resource_ids = map entityKey resources

    authorsMap <- handlerToWidget $ runDB (fetchResourceAuthorsInDB resource_ids)

    (is_logged_in, favs, grokked) <- handlerToWidget $
        maybeAuthId >>= \case
            Nothing  -> return (False, mempty, mempty)
            Just uid -> runDB $ (,,)
                <$> pure True
                <*> (S.fromList <$> fetchFavoriteResourceIdsInDB resource_ids uid)
                <*> (S.fromList <$> fetchGrokkedResourceIdsInDB  resource_ids uid)

    $(widgetFile "resource-list")

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
