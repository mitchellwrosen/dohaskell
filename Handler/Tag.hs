module Handler.Tag where

import Import

getTagR :: Text -> Handler Html
getTagR t = do
    Entity tagId _ <- runDB $ getBy404 (UniqueTag t)
    resources      <- runDB $ getResourcesWithTagId tagId
    defaultLayout $ do
        setTitle "Tag"
        $(widgetFile "tag")

getResourcesWithTagId :: TagId -> SqlPersistT Handler [Entity Resource]
getResourcesWithTagId tagId = 
    select $ from $ \(resource, resourceTag) -> do
        where_ (resource^.ResourceId ==. resourceTag^.ResourceTagResourceId
            &&. resourceTag^.ResourceTagTagId ==. val tagId)
        return resource
