module View.Browse
    ( BrowseByLink(..)
    , browseBarWidget
    , pageWidget
    , pageWidgetEmbed
    , resourceListWidget
    , sortBarWidget
    , sortResBarWidget
    ) where

import Import

import Handler.Utils
import Model.Browse
import Model.Resource
import Model.User

import qualified Data.Map     as M
import qualified Data.Set     as S
import qualified Data.Text    as T
import           Text.Cassius (cassiusFile)
import           Text.Julius  (juliusFile)
import           Text.Hamlet  (hamletFile)

boldIfEq :: Eq a => a -> a -> Text
boldIfEq x y | x == y = "bold"
boldIfEq _ _          = "normal"

browseBarWidget :: BrowseByLink -> Widget
browseBarWidget browse_by_link = do
    [whamlet|
      <div .bar>browse by: #
        <a .bar-link #br-auth href=@{BrowseAuthorsR}>author
        |
        <a .bar-link #br-tag href=@{BrowseTagsR}>tag
        |
        <a .bar-link #br-coll href=@{BrowseCollectionsR}>collection
        |
        <a .bar-link #br-type href=@{BrowseTypesR}>type
        |
        <a .bar-link #br-res href=@{BrowseResourcesR}>list all
    |]
    topBarCSS
    toWidget [cassius|
      #br-auth
        font-weight: #{boldIfEq browse_by_link BrowseByAuthorLink}

      #br-coll
        font-weight: #{boldIfEq browse_by_link BrowseByCollectionLink}

      #br-res
        font-weight: #{boldIfEq browse_by_link BrowseByResourceLink}

      #br-tag
        font-weight: #{boldIfEq browse_by_link BrowseByTagLink}

      #br-type
        font-weight: #{boldIfEq browse_by_link BrowseByTypeLink}
    |]

sortBarWidget :: Text -> SortBy -> Widget
sortBarWidget text SortByAZ = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-down href=@?{(route, addGetParam ("sort", T.pack (show SortByCountDown)) params)}>count#
          <span .arr>&#9660;
        |
        <a .bar-link #so-year-down href=@?{(route, addGetParam ("sort", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9660;
    |]
    topBarCSS
    toWidget [cassius|
      #so-az
        font-weight: bold
    |]
sortBarWidget text SortByCountUp = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-down href=@?{(route, addGetParam ("sort", T.pack (show SortByCountDown)) params)}>count#
          <span .arr>&#9650;
        |
        <a .bar-link #so-year-down href=@?{(route, addGetParam ("sort", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9660;
    |]
    topBarCSS
    toWidget [cassius|
      #so-count-down
        font-weight: bold
    |]
sortBarWidget text SortByCountDown = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-up href=@?{(route, addGetParam ("sort", T.pack (show SortByCountUp)) params)}>count#
          <span .arr>&#9660;
        |
        <a .bar-link #so-year-down href=@?{(route, addGetParam ("sort", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9660;
    |]
    topBarCSS
    toWidget [cassius|
      #so-count-up
        font-weight: bold
    |]
sortBarWidget text SortByYearUp = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-down href=@?{(route, addGetParam ("sort", T.pack (show SortByCountDown)) params)}>count#
          <span .arr>&#9660;
        |
        <a .bar-link #so-year-down href=@?{(route, addGetParam ("sort", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9650;
    |]
    topBarCSS
    toWidget [cassius|
      #so-year-down
        font-weight: bold
    |]
sortBarWidget text SortByYearDown = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-down href=@?{(route, addGetParam ("sort", T.pack (show SortByCountDown)) params)}>count#
          <span .arr>&#9660;
        |
        <a .bar-link #so-year-up href=@?{(route, addGetParam ("sort", T.pack (show SortByYearUp)) params)}>year#
          <span .arr>&#9660;
    |]
    topBarCSS
    toWidget [cassius|
      #so-year-up
        font-weight: bold
    |]
sortBarWidget text SortByRecentlyAdded = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar>sort #{text} by: #
        <a .bar-link #so-az href=@?{(route, addGetParam ("sort", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-count-down href=@?{(route, addGetParam ("sort", T.pack (show SortByCountDown)) params)}>count#
          <span .arr>&#9660;
        |
        <a .bar-link #so-year-up href=@?{(route, addGetParam ("sort", T.pack (show SortByYearUp)) params)}>year#
          <span .arr>&#9660;
    |]
    topBarCSS
    toWidget [cassius|
      #so-recently-added
        font-weight: bold
    |]

sortResBarWidget :: SortBy -> Widget
sortResBarWidget SortByYearUp = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar .sort-res-bar>sort resources by: #
        <a .bar-link #so-res-az href=@?{(route, addGetParam ("sort-res", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-res-year-down href=@?{(route, addGetParam ("sort-res", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9650;
        |
        <a .bar-link #so-res-recently-added href=@?{(route, addGetParam ("sort-res", T.pack (show SortByRecentlyAdded)) params)}>recently added
    |]
    sortResBarCSS
    toWidget [cassius|
      #so-res-year-down
        font-weight: bold
    |]
sortResBarWidget SortByYearDown = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar .sort-res-bar>sort resources by: #
        <a .bar-link #so-res-az href=@?{(route, addGetParam ("sort-res", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-res-year-up href=@?{(route, addGetParam ("sort-res", T.pack (show SortByYearUp)) params)}>year#
          <span .arr>&#9660;
        |
        <a .bar-link #so-res-recently-added href=@?{(route, addGetParam ("sort-res", T.pack (show SortByRecentlyAdded)) params)}>recently added
    |]
    sortResBarCSS
    toWidget [cassius|
      #so-res-year-up
        font-weight: bold
    |]
sortResBarWidget SortByRecentlyAdded = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar .sort-res-bar>sort resources by: #
        <a .bar-link #so-res-az href=@?{(route, addGetParam ("sort-res", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-res-year-down href=@?{(route, addGetParam ("sort-res", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9660;
        |
        <a .bar-link #so-res-recently-added href=@?{(route, addGetParam ("sort-res", T.pack (show SortByRecentlyAdded)) params)}>recently added
    |]
    sortResBarCSS
    toWidget [cassius|
      #so-res-recently-added
        font-weight: bold
    |]
sortResBarWidget _ = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      <div .bar .sort-res-bar>sort resources by: #
        <a .bar-link #so-res-az href=@?{(route, addGetParam ("sort-res", T.pack (show SortByAZ)) params)}>a-z
        |
        <a .bar-link #so-res-year-down href=@?{(route, addGetParam ("sort-res", T.pack (show SortByYearDown)) params)}>year#
          <span .arr>&#9660;
        |
        <a .bar-link #so-res-recently-added href=@?{(route, addGetParam ("sort-res", T.pack (show SortByRecentlyAdded)) params)}>recently added
    |]
    sortResBarCSS
    toWidget [cassius|
      #so-res-az
        font-weight: bold
    |]

-- | CSS that applies to browse/sort/sort resources bars.
topBarCSS :: Widget
topBarCSS = toWidget
    [cassius|
      .bar
        font-size: 1.1em
        font-variant: small-caps
        height: 1.1em
        line-height: 1.1em

      .bar-link
        color: #069

      a.bar-link:hover
        text-decoration: none

      .arr
        font-size: 0.7em
    |]


-- | CSS that applies to all sort resource bars (topBarCSS + bottom margin + bottom border)
sortResBarCSS :: Widget
sortResBarCSS = do
    topBarCSS
    toWidget [cassius|
      .sort-res-bar
        border-bottom: 1px solid black
        margin-bottom: 4px
    |]

resourceListWidget :: [Entity Resource] -> Widget
resourceListWidget resources = do
    let resource_ids = map entityKey resources

    authorsMap <- handlerToWidget $ runDB (fetchResourceAuthorsInDB resource_ids)

    (is_logged_in, grokked) <- handlerToWidget $
        maybeAuthId >>= \case
            Nothing  -> return (False, mempty)
            Just user_id -> runDB $ (,)
                <$> pure True
                <*> (S.fromList <$> fetchGrokkedResourceIdsInDB user_id resource_ids)

    toWidget $(hamletFile  "templates/resource-list.hamlet")
    toWidget $(cassiusFile "templates/resource-list.cassius")
    if is_logged_in
        then toWidget $(juliusFile "templates/resource-list-logged-in.julius")
        else toWidget $(juliusFile "templates/resource-list-not-logged-in.julius")

pageWidget :: Maybe Int64 -> Maybe Int64 -> Widget
pageWidget mprev mnext = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    [whamlet|
      $maybe prev <- mprev
        <a href=@?{(route, addGetParam ("page", T.pack (show prev)) params)}>Prev
      $maybe next <- mnext
        <a href=@?{(route, addGetParam ("page", T.pack (show next)) params)}>Next
    |]

pageWidgetEmbed :: Maybe Int64 -> Maybe Int64 -> Widget
pageWidgetEmbed mprev mnext = do
    (route, params) <- handlerToWidget getCurrentRouteWithGetParams
    case mprev of
        Nothing -> pure ()
        Just prev -> do
            [whamlet|
              $maybe _ <- mprev
                <a .prev-page>Prev
            |]
            toWidget [julius|
              $(document).ready(function() {
                $('.prev-page').click(function() {
                  $(this).parent().load('@?{(route, addGetParam ("page", T.pack (show prev)) params)}');
                });
              });
            |]
    case mnext of
        Nothing -> pure ()
        Just next -> do
            [whamlet|
              $maybe _ <- mnext
                <a .next-page>Next
            |]
            toWidget [julius|
              $(document).ready(function() {
                $('.next-page').click(function() {
                  $(this).parent().load('@?{(route, addGetParam ("page", T.pack (show next)) params)}');
                });
              });
            |]
    toWidget [cassius|
      a.prev-page:hover
        cursor: pointer

      a.next-page:hover
        cursor: pointer
    |]
