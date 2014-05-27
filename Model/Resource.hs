module Model.Resource 
    ( getResourceComments
    , getResourceTags
    , getResourceTagsWithIds
    , updateResource
    ) where

import Import
import Foundation

getResourceTags :: ResourceId -> YesodDB App [Tag]
getResourceTags = fmap (map entityVal) . getResourceTagsWithIds

-- Longer name because it's probably more likely that we don't care about the
-- tags' ids.
getResourceTagsWithIds :: ResourceId -> YesodDB App [Entity Tag]
getResourceTagsWithIds resId =
    select $ from $ \(tag, resourceTag) -> do
        where_ (tag^.TagId ==. resourceTag^.ResourceTagTagId
            &&. resourceTag^.ResourceTagResourceId ==. val resId)
        return tag

getResourceComments :: ResourceId -> YesodDB App [Entity Comment]
getResourceComments resId =
    select $ from $ \comment -> do
        where_ (comment^.CommentResourceId ==. val resId)
        orderBy [asc (comment^.CommentPosted)]
        return comment

-- Adjust Resource's title and type. Add all Tags to the database, collecting
-- their ids. Remove all ResourceTag relations for the Resource, and add back
-- new relations between the Resource and each Tag id collected.
updateResource :: ResourceId -> Text -> ResourceType -> [Tag] -> YesodDB App ()
updateResource resId title typ tags = do
    update $ \resource -> do
        set resource [ ResourceTitle =. val title
                     , ResourceType  =. val typ
                     ]
        where_ (resource^.ResourceId ==. val resId)
    tagIds <- mapM insertBy' tags
    delete $ from $ \resourceTag -> do
        where_ (resourceTag^.ResourceTagResourceId ==. val resId)
    mapM_ (insertUnique . ResourceTag resId) tagIds
