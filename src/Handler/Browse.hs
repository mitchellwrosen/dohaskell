module Handler.Browse where

import Import

import Handler.Utils
import Model.Author
import Model.Browse
import Model.Resource
import Model.Tag
import Model.User
import Model.Utils
import View.Browse

import           Data.List    (sortBy)
import qualified Data.Map     as M
import           Data.Maybe   (isJust)
import           Text.Blaze   (ToMarkup)
import           Text.Cassius (cassiusFile)
import           Text.Julius  (juliusFile)
import           Text.Hamlet  (hamletFile)

-- | Look up GET param for sorting, default to alphabetical.
-- Careful changing the strings here - they are referenced stringly
-- around the code.
lookupSortByParam :: Handler SortBy
lookupSortByParam = lookupGetParam "sort" >>= \case
    Just "paucity"     -> return SortByCountUp
    Just "obscurity"   -> return SortByCountUp
    Just "prolificity" -> return SortByCountDown
    Just "profusion"   -> return SortByCountDown
    Just "senescence"  -> return SortByEarliest
    Just "pubescence"  -> return SortByLatest
    _                  -> return SortByAZ

getHomeR :: Handler Html
getHomeR = browseTagsHandler "dohaskell: tagged Haskell learning resources"

getAuthorR, getTagR :: Text -> Handler Html
getAuthorR text = getResources (fetchResourcesWithAuthorDB text) ("dohaskell | by " <> text)
getTagR    text = getResources (fetchResourcesWithTagDB text)    ("dohaskell | " <> text)

getTypeR :: Text -> Handler Html
getTypeR text = case shortReadResourceTypePlural text of
    Nothing  -> notFound
    Just typ -> getResources (fetchResourcesWithTypeDB typ) ("dohaskell | the " <> text)

-- | Abstract getAuthorR, getTagR, and getTypeR. Assumes the resources grabbed from the
-- database are unsorted.
getResources :: ToMarkup markup => YesodDB App [Entity Resource] -> markup -> Handler Html
getResources get_resources title = do
    resources <- sortBy (orderAlphabeticIgnoreCase resourceTitle) <$> runDB get_resources
    is_embed  <- isJust <$> lookupGetParam "embed"
    let body = do
          setTitle (toHtml title)
          resourceListWidget resources
    if is_embed
        then do
            pc <- widgetToPageContent body
            giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        else defaultLayout body

getBrowseAuthorsR :: Handler Html
getBrowseAuthorsR = do
    muid <- maybeAuthId
    (unsorted_authors, year_ranges, total_counts, mgrokked_counts) <- runDB $ (,,,)
        <$> fetchAllAuthorsDB
        <*> fetchAuthorYearRangesDB
        <*> fetchAuthorResourceCountsDB
        <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByAuthorDB) muid

    sort_by <- lookupSortByParam
    let order_func = case sort_by of
                         SortByAZ        -> orderAlphabeticIgnoreCase authorName
                         SortByCountUp   -> orderCountUp authorName total_counts
                         SortByCountDown -> orderCountDown authorName total_counts
                         SortByEarliest  -> orderEarliest authorName year_ranges
                         SortByLatest    -> orderLatest authorName year_ranges
        authors = sortBy order_func unsorted_authors

    defaultLayout $ do
        setTitle "dohaskell | browse authors"
        browseBarWidget BrowseByAuthorLink
        sortAuthorBarWidget sort_by
        giveUrlRenderer $(hamletFile "templates/browse-authors.hamlet") >>= toWidgetBody
        toWidget $(cassiusFile "templates/browse-list.cassius")
        let path_piece = String "/author/"
         in toWidget $(juliusFile "templates/browse-list.julius")

getBrowseResourcesR :: Handler Html
getBrowseResourcesR = do
    unsorted_resources <- runDB fetchAllResourcesDB

    let resources = sortBy (orderAlphabeticIgnoreCase resourceTitle) unsorted_resources

    defaultLayout $ do
        setTitle "dohaskell | browse resources"
        browseBarWidget BrowseByResourceLink
        resourceListWidget resources

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

    sort_by <- lookupSortByParam
    let order_func = case sort_by of
                         SortByAZ        -> orderAlphabeticIgnoreCase tagTag
                         SortByCountUp   -> orderCountUp tagTag total_counts
                         SortByCountDown -> orderCountDown tagTag total_counts
                         SortByEarliest  -> orderEarliest tagTag year_ranges
                         SortByLatest    -> orderLatest tagTag year_ranges
        tags = sortBy order_func unsorted_tags

    defaultLayout $ do
        setTitle title
        browseBarWidget BrowseByTagLink
        sortTagBarWidget sort_by
        giveUrlRenderer $(hamletFile "templates/browse-tags.hamlet") >>= toWidgetBody
        toWidget $(cassiusFile "templates/browse-list.cassius")
        let path_piece = String "/tag/"
         in toWidget $(juliusFile "templates/browse-list.julius")

getBrowseTypesR :: Handler Html
getBrowseTypesR = do
    muid <- maybeAuthId
    (typeCounts, mgrokkedCounts) <- runDB $ (,)
        <$> getTypeCounts
        <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByTypeDB) muid

    defaultLayout $ do
        setTitle "dohaskell | browse types"
        browseBarWidget BrowseByTypeLink
        typeListWidget typeCounts mgrokkedCounts
