module Handler.Browse where

import Import

import Model.Author
import Model.Browse
import Model.Resource
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
    resources <- sortBy (alphabeticIgnoreCase resourceTitle) <$> runDB get_resources
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
    (authors, year_ranges, total_counts, mgrokked_counts) <- runDB $ (,,,)
        <$> fetchAllAuthorsDB
        <*> fetchAuthorYearRangesDB
        <*> fetchAuthorResourceCountsDB
        <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByAuthorDB) muid

    defaultLayout $ do
        setTitle "dohaskell | browse authors"
        browseBarWidget BrowseByAuthorLink
        giveUrlRenderer $(hamletFile "templates/browse-authors.hamlet") >>= toWidgetBody
        toWidget $(cassiusFile "templates/browse-list.cassius")
        let path_piece = String "/author/"
         in toWidget $(juliusFile "templates/browse-list.julius")

getBrowseResourcesR :: Handler Html
getBrowseResourcesR = do
    resources <- runDB fetchAllResourcesDB
    defaultLayout $ do
        setTitle "dohaskell | browse resources"
        browseBarWidget BrowseByResourceLink
        resourceListWidget resources

getBrowseTagsR :: Handler Html
getBrowseTagsR = browseTagsHandler "dohaskell | browse tags"

browseTagsHandler :: Html -> Handler Html
browseTagsHandler title = do
    muid <- maybeAuthId
    (tags, tagCounts, mgrokkedCounts) <- runDB $ (,,)
        <$> getAllTags
        <*> getTagCounts
        <*> maybe (return Nothing) (fmap Just . fetchGrokkedCountsByTagDB) muid

    defaultLayout $ do
        setTitle title
        browseBarWidget BrowseByTagLink
        tagListWidget tags tagCounts mgrokkedCounts

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
