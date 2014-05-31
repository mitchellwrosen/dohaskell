module Handler.Tag where

import Import

getTagR :: Text -> Handler Html
getTagR text = do
    Entity tagId _ <- runDB $ getBy404 (UniqueTagText text)
    resources      <- runDB $ getResourcesWithTagId tagId
    defaultLayout $ do
        setTitle "Tag"
        $(widgetFile "resource-list")

getResourcesWithTagId :: TagId -> SqlPersistT Handler [Entity Resource]
getResourcesWithTagId tagId = 
    select $ from $ \(resource, resourceTag) -> do
        where_ (resource^.ResourceId ==. resourceTag^.ResourceTagResId
            &&. resourceTag^.ResourceTagTagId ==. val tagId)
        return resource
