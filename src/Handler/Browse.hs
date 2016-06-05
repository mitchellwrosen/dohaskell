-- FIXME: lots of code duplication in this module

module Handler.Browse where

import Import

import Handler.Utils
import Model.Author
import Model.Browse
import Model.Collection
import Model.Resource
import Model.Tag
import Model.User
import Model.Utils
import View.Browse

import Database.Esqueleto (asc, desc)
import Data.Aeson         (Value(..))
import Text.Blaze         (ToMarkup)
import Text.Hamlet        (hamletFile)

import qualified Data.Map  as M
import qualified Data.Text as T

-- | Look up GET param for sorting, default to alphabetical.
lookupSortByParam :: Handler SortBy
lookupSortByParam = lookupGetParam "sort" >>= \case
  Just "count-up"   -> return SortByCountUp
  Just "count-down" -> return SortByCountDown
  Just "year-up"    -> return SortByYearUp
  Just "year-down"  -> return SortByYearDown
  _                 -> return SortByAZ

-- | Look up GET param for sorting resources, default to alphabetical.
lookupSortResByParam :: Handler SortBy
lookupSortResByParam = lookupGetParam "sort-res" >>= \case
  Just "year-up"        -> return SortByYearUp
  Just "year-down"      -> return SortByYearDown
  Just "recently-added" -> return SortByRecentlyAdded
  _                     -> return SortByAZ

-- | Look up GET param for page; given the page size, return page num, limit,
-- and offset.
lookupPageParam :: Int64 -> Handler (Int64, Int64, Int64)
lookupPageParam page_size = do
  page <- fromMaybe 1 . (\p -> p >>= readMay) <$> lookupGetParam "page"
  let lim = page_size + 1
      off = (page-1) * page_size
  pure (page, lim, off)

resourceOrder :: SortBy -> Entity Resource -> Entity Resource -> Ordering
resourceOrder = \case
  SortByYearUp        -> orderResourceYearUp
  SortByYearDown      -> orderResourceYearDown
  SortByRecentlyAdded -> orderResourceRecentlyAdded
  _                   -> orderAlphabeticIgnoreCase (resourceTitle . entityVal)
 where
  orderResourceYearUp :: Entity Resource -> Entity Resource -> Ordering
  orderResourceYearUp (Entity _ x) (Entity _ y) =
    compareEarliest (resourcePublished x) (resourcePublished y) <>
    orderAlphabeticIgnoreCase resourceTitle x y
   where
    compareEarliest :: Maybe Int -> Maybe Int -> Ordering
    compareEarliest (Just n) (Just m) = compare n m
    compareEarliest Nothing  (Just _) = GT -- Nothing means no year, so put it at the bottom
    compareEarliest (Just _) Nothing  = LT
    compareEarliest _        _        = EQ

  orderResourceYearDown :: Entity Resource -> Entity Resource -> Ordering
  orderResourceYearDown (Entity _ x) (Entity _ y) =
    compareLatest (resourcePublished x) (resourcePublished y) <> orderAlphabeticIgnoreCase resourceTitle x y
   where
    compareLatest :: Maybe Int -> Maybe Int -> Ordering
    compareLatest (Just n) (Just m) = compare m n
    compareLatest Nothing  (Just _) = GT -- Nothing means no year, so put it at the bottom
    compareLatest (Just _) Nothing  = LT
    compareLatest _        _        = EQ

  orderResourceRecentlyAdded :: Entity Resource -> Entity Resource -> Ordering
  orderResourceRecentlyAdded (Entity uidx _) (Entity uidy _) =
    compare uidy uidx -- Ids as substitutes for submission dates

vshow :: Show a => a -> Value
vshow = String . T.pack . show

--------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = browseTagsHandler "dohaskell: tagged Haskell learning resources"

getAuthorR, getCollectionR, getTagR :: Text -> Handler Html
getAuthorR     text = getResources (fetchResourcesByAuthorDB text)     ("dohaskell | by " <> text)
getCollectionR text = getResources (fetchResourcesInCollectionDB text) ("dohaskell | in " <> text)
getTagR        text = getResources (fetchResourcesWithTagDB text)      ("dohaskell | " <> text)

getTypeR :: Text -> Handler Html
getTypeR text = case shortReadResourceTypePlural text of
  Nothing  -> notFound
  Just typ -> getResources (fetchResourcesWithTypeDB typ) ("dohaskell | the " <> text)

-- | Abstract getAuthorR, getCollectionR, getTagR, and getTypeR. Assumes the
-- resources grabbed from the database are unsorted.
getResources
  :: ToMarkup markup
  => (Int64 -> Int64 -> YesodDB App [Entity Resource])
  -> markup
  -> Handler Html
getResources get_resources title = do
  sort_res_by <- lookupSortResByParam

  let page_size = 10

  (page, lim, off) <- lookupPageParam page_size
  unsorted_resources <- runDB (get_resources lim off)

  let mprev = if page > 1
                then Just (page-1)
                else Nothing

      mnext = if length unsorted_resources > page_size
                then Just (page+1)
                else Nothing

      resources = sortBy (resourceOrder sort_res_by) (take page_size unsorted_resources)

  is_embed <- isJust <$> lookupGetParam "embed"

  if is_embed
    then do
      pc <- widgetToPageContent $ do
        setTitle (toHtml title)
        resourceListWidget resources
        pageWidgetEmbed mprev mnext
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    else defaultLayout $ do
      setTitle (toHtml title)
      sortResBarWidget sort_res_by
      resourceListWidget resources
      pageWidget mprev mnext

getBrowseAuthorsR :: Handler Html
getBrowseAuthorsR = do
  muid <- maybeAuthId
  (unsorted_authors, year_ranges, total_counts, mgrokked_counts) <- runDB $ (,,,)
    <$> fetchAllAuthorsDB
    <*> fetchAuthorYearRangesDB
    <*> fetchAuthorResourceCountsDB
    <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByAuthorDB) muid

  sort_by     <- lookupSortByParam
  sort_res_by <- lookupSortResByParam
  let order_func       = case sort_by of
                           SortByAZ            -> orderAlphabeticIgnoreCase (authorName . entityVal)
                           SortByCountUp       -> orderCountUp (authorName . entityVal) entityKey total_counts
                           SortByCountDown     -> orderCountDown (authorName . entityVal) entityKey total_counts
                           SortByYearUp        -> orderYearUp (authorName . entityVal) entityKey year_ranges
                           SortByYearDown      -> orderYearDown (authorName . entityVal) entityKey year_ranges
                           SortByRecentlyAdded -> orderAlphabeticIgnoreCase (authorName . entityVal)
      entities         = sortBy order_func unsorted_authors
      get_maps_key     = entityKey
      get_permalink    = AuthorR . authorName . entityVal
      show_entity      = authorName . entityVal
      path_piece       = String "/author/"
      sort_res_by_text = vshow sort_res_by

  defaultLayout $ do
    setTitle "dohaskell | browse authors"
    browseBarWidget BrowseByAuthorLink
    sortBarWidget "authors" sort_by
    sortResBarWidget sort_res_by
    $(widgetFile "browse")

getBrowseCollectionsR :: Handler Html
getBrowseCollectionsR = do
  muid <- maybeAuthId
  (unsorted_collections, year_ranges, total_counts, mgrokked_counts) <- runDB $ (,,,)
    <$> fetchAllCollectionsDB
    <*> fetchCollectionYearRangesDB
    <*> fetchCollectionResourceCountsDB
    <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByCollectionDB) muid

  sort_by     <- lookupSortByParam
  sort_res_by <- lookupSortResByParam
  let order_func       = case sort_by of
                           SortByAZ            -> orderAlphabeticIgnoreCase (collectionName . entityVal)
                           SortByCountUp       -> orderCountUp   (collectionName . entityVal) entityKey total_counts
                           SortByCountDown     -> orderCountDown (collectionName . entityVal) entityKey total_counts
                           SortByYearUp        -> orderYearUp    (collectionName . entityVal) entityKey year_ranges
                           SortByYearDown      -> orderYearDown  (collectionName . entityVal) entityKey year_ranges
                           SortByRecentlyAdded -> orderAlphabeticIgnoreCase (collectionName . entityVal)
      entities         = sortBy order_func unsorted_collections
      get_maps_key     = entityKey
      get_permalink    = CollectionR . collectionName . entityVal
      show_entity      = collectionName . entityVal
      path_piece       = String "/collection/"
      sort_res_by_text = vshow sort_res_by

  defaultLayout $ do
    setTitle "dohaskell | browse collections"
    browseBarWidget BrowseByCollectionLink
    sortBarWidget "collections" sort_by
    sortResBarWidget sort_res_by
    $(widgetFile "browse")

getBrowseResourcesR :: Handler Html
getBrowseResourcesR = do
  let page_size = 100

  (page, lim, off) <- lookupPageParam page_size

  sort_res_by <- lookupSortResByParam
  let order = case sort_res_by of
                SortByRecentlyAdded -> desc
                _                   -> asc

  unsorted_resources <- runDB (fetchAllResourcesDB order lim off)

  let mprev = if page > 1
                then Just (page-1)
                else Nothing

      mnext = if length unsorted_resources > page_size
                then Just (page+1)
                else Nothing

      resources = sortBy (resourceOrder sort_res_by) (take page_size unsorted_resources)


  defaultLayout $ do
    setTitle "dohaskell | browse resources"
    browseBarWidget BrowseByResourceLink
    sortResBarWidget sort_res_by
    resourceListWidget resources
    pageWidget mprev mnext

getBrowseTagsR :: Handler Html
getBrowseTagsR = browseTagsHandler "dohaskell | browse tags"

browseTagsHandler :: Html -> Handler Html
browseTagsHandler title = do
  muid <- maybeAuthId
  (unsorted_tags, year_ranges, total_counts, mgrokked_counts) <- runDB $ (,,,)
    <$> fetchAllTagsDB
    <*> fetchTagYearRangesDB
    <*> fetchTagCountsDB
    <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByTagDB) muid

  sort_by     <- lookupSortByParam
  sort_res_by <- lookupSortResByParam
  let order_func       = case sort_by of
                           SortByAZ            -> orderAlphabeticIgnoreCase (tagName . entityVal)
                           SortByCountUp       -> orderCountUp   (tagName . entityVal) entityKey total_counts
                           SortByCountDown     -> orderCountDown (tagName . entityVal) entityKey total_counts
                           SortByYearUp        -> orderYearUp    (tagName . entityVal) entityKey year_ranges
                           SortByYearDown      -> orderYearDown  (tagName . entityVal) entityKey year_ranges
                           SortByRecentlyAdded -> orderAlphabeticIgnoreCase (tagName . entityVal)
      entities         = sortBy order_func unsorted_tags
      get_maps_key     = entityKey
      get_permalink    = TagR . tagName . entityVal
      show_entity      = tagName . entityVal
      path_piece       = String "/tag/"
      sort_res_by_text = vshow sort_res_by

  defaultLayout $ do
    setTitle title
    browseBarWidget BrowseByTagLink
    sortBarWidget "tags" sort_by
    sortResBarWidget sort_res_by
    $(widgetFile "browse")

getBrowseTypesR :: Handler Html
getBrowseTypesR = do
  muid <- maybeAuthId
  (total_counts, year_ranges, mgrokked_counts) <- runDB $ (,,)
    <$> fetchResourceTypeCountsDB
    <*> fetchResourceTypeYearRangesDB
    <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByTypeDB) muid

  sort_by     <- lookupSortByParam
  sort_res_by <- lookupSortResByParam
  let order_func       = case sort_by of
                           SortByAZ            -> orderAlphabeticIgnoreCase shortDescResourceTypePlural
                           SortByCountUp       -> orderCountUp              shortDescResourceTypePlural id total_counts
                           SortByCountDown     -> orderCountDown            shortDescResourceTypePlural id total_counts
                           SortByYearUp        -> orderYearUp               shortDescResourceTypePlural id year_ranges
                           SortByYearDown      -> orderYearDown             shortDescResourceTypePlural id year_ranges
                           SortByRecentlyAdded -> orderAlphabeticIgnoreCase shortDescResourceTypePlural
      entities         = sortBy order_func [minBound..maxBound]
      get_maps_key     = id
      get_permalink    = TypeR . shortDescResourceTypePlural
      show_entity      = shortDescResourceTypePlural
      path_piece       = String "/type/"
      sort_res_by_text = vshow sort_res_by

  defaultLayout $ do
    setTitle "dohaskell | browse types"
    browseBarWidget BrowseByTypeLink
    sortBarWidget "types" sort_by
    sortResBarWidget sort_res_by
    $(widgetFile "browse")
