module View.Browse
    ( BrowseByLink(..)
    , browseBarWidget
    , resourceListWidget
    , sortBarWidget
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

sortBarWidget :: Text -> SortBy -> Widget
sortBarWidget text SortByAZ = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort #{text} by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-down href=@?{(route, [("sort", "count-down")])}>count
          <span .arr>&#9660;
        |
        <a .sort-link #so-year-down href=@?{(route, [("sort", "year-down")])}>year
          <span .arr>&#9660;
    |]
    sortBarCSS
    toWidget [cassius|
      #so-az
        font-weight: bold
    |]
sortBarWidget text SortByCountUp = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort #{text} by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-down href=@?{(route, [("sort", "count-down")])}>count#
          <span .arr>&#9650;
        |
        <a .sort-link #so-year-down href=@?{(route, [("sort", "year-down")])}>year
          <span .arr>&#9660;
    |]
    sortBarCSS
    toWidget [cassius|
      #so-count-down
        font-weight: bold
    |]
sortBarWidget text SortByCountDown = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort #{text} by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-up href=@?{(route, [("sort", "count-up")])}>count#
          <span .arr>&#9660;
        |
        <a .sort-link #so-year-down href=@?{(route, [("sort", "year-down")])}>year
          <span .arr>&#9660;
    |]
    sortBarCSS
    toWidget [cassius|
      #so-count-up
        font-weight: bold
    |]
sortBarWidget text SortByYearUp = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort #{text} by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-down href=@?{(route, [("sort", "count-down")])}>count#
          <span .arr>&#9660;
        |
        <a .sort-link #so-year-down href=@?{(route, [("sort", "year-down")])}>year
          <span .arr>&#9650;
    |]
    sortBarCSS
    toWidget [cassius|
      #so-year-down
        font-weight: bold
    |]
sortBarWidget text SortByYearDown = do
    Just route <- handlerToWidget getCurrentRoute
    [whamlet|
      <div .sort-bar>sort #{text} by: #
        <a .sort-link #so-az href=@?{(route, [("sort", "a-z")])}>a-z
        |
        <a .sort-link #so-count-down href=@?{(route, [("sort", "count-down")])}>count#
          <span .arr>&#9660;
        |
        <a .sort-link #so-year-up href=@?{(route, [("sort", "year-up")])}>year
          <span .arr>&#9660;
    |]
    sortBarCSS
    toWidget [cassius|
      #so-year-up
        font-weight: bold
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

      a.sort-link:hover
        text-decoration: none

      .arr
        font-size: 0.7em
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
