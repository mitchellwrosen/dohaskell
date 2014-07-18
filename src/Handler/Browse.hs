module Handler.Browse where

import Import

import Model.Browse
import Model.Resource
import Model.User
import Model.Utils
import View.Browse

import Data.List   (sortBy)
import Data.Maybe  (isJust)
import Text.Blaze  (ToMarkup)
import Text.Hamlet (hamletFile)

getHomeR :: Handler Html
getHomeR = browseTagsHandler "dohaskell: tagged Haskell learning resources"

getAuthorR, getTagR :: Text -> Handler Html
getAuthorR text = getResources (getResourcesWithAuthor text) ("dohaskell | by " <> text)
getTagR    text = getResources (getResourcesWithTag text)    ("dohaskell | " <> text)

getTypeR :: Text -> Handler Html
getTypeR text = case shortReadResourceTypePlural text of
    Nothing  -> notFound
    Just typ -> getResources (getResourcesWithType typ) ("dohaskell | the " <> text)

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
    (authors, authorCounts, mgrokkedCounts) <- runDB $ (,,)
        <$> getAllAuthors
        <*> getAuthorCounts
        <*> maybe (return Nothing) (fmap Just . getGrokkedCountsByAuthor) muid
    defaultLayout $ do
        setTitle "dohaskell | browse authors"
        addJqueryRemote
        browseBarWidget BrowseByAuthorLink
        authorListWidget authors authorCounts mgrokkedCounts

getBrowseResourcesR :: Handler Html
getBrowseResourcesR = do
    resources <- runDB getAllResources
    defaultLayout $ do
        setTitle "dohaskell | browse resources"
        addJqueryRemote
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
        <*> maybe (return Nothing) (fmap Just . getGrokkedCountsByTag) muid

    defaultLayout $ do
        setTitle title
        addJqueryRemote
        browseBarWidget BrowseByTagLink
        tagListWidget tags tagCounts mgrokkedCounts

getBrowseTypesR :: Handler Html
getBrowseTypesR = do
    muid <- maybeAuthId
    (typeCounts, mgrokkedCounts) <- runDB $ (,)
        <$> getTypeCounts
        <*> maybe (return Nothing) (fmap Just . getGrokkedCountsByType) muid

    defaultLayout $ do
        setTitle "dohaskell | browse types"
        addJqueryRemote
        browseBarWidget BrowseByTypeLink
        typeListWidget typeCounts mgrokkedCounts
