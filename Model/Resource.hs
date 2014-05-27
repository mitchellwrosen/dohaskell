module Model.Resource 
    ( getResourceComments
    , getResourceTags
    ) where

import Import

getResourceTags :: ResourceId -> SqlPersistT Handler [Entity Tag]
getResourceTags resId = 
    select $ from $ \(tag, resourceTag) -> do
        where_ (tag^.TagId ==. resourceTag^.ResourceTagTagId
            &&. resourceTag^.ResourceTagResourceId ==. val resId)
        return tag

getResourceComments :: ResourceId -> SqlPersistT Handler [Entity Comment]
getResourceComments resId =
    select $ from $ \comment -> do
        where_ (comment^.CommentResource ==. val resId)
        return comment
