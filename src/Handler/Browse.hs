module Handler.Browse where

import Import

import Model.AuthorAndTag
import Model.Resource
import Model.User
import View.Browse

getHomeR :: Handler Html
getHomeR = browseTagsHandler "dohaskell: tagged Haskell learning resources"

getBrowseAuthorsR :: Handler Html
getBrowseAuthorsR = do
    muid <- maybeAuthId
    (authors, authorCounts, mgrokkedCounts) <- runDB $ (,,)
        <$> getAllAuthors
        <*> getAuthorCounts
        <*> maybe (return Nothing) (fmap Just . getGrokkedCountsByAuthor) muid
    defaultLayout $ do
        setTitle "dohaskell | browse authors"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        browseBarWidget AuthorsLink
        authorListWidget authors authorCounts mgrokkedCounts

getBrowseResourcesR :: Handler Html
getBrowseResourcesR = do
    resources <- runDB $ getAllResources
    defaultLayout $ do
        setTitle "dohaskell | browse resources"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        browseBarWidget ResourcesLink
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
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        browseBarWidget TagsLink
        tagListWidget tags tagCounts mgrokkedCounts
