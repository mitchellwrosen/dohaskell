module Model.Resource 
    ( getResourceComments
    , getResourceTags
    , getResourceTagsWithIds
    , updateResource
    ) where

import Import

import qualified Database.Persist as P

import Model.ResourceTag (getResourceTagsByResId)

getResourceComments :: ResourceId -> YesodDB App [Entity Comment]
getResourceComments resId =
    select $ 
        from $ \comment -> do
        where_ (comment^.CommentResId ==. val resId)
        orderBy [asc (comment^.CommentPosted)]
        return comment

getResourceTags :: ResourceId -> YesodDB App [Tag]
getResourceTags = fmap (map entityVal) . getResourceTagsWithIds

-- Longer name because it's probably more likely that we don't care about the
-- tags' ids.
getResourceTagsWithIds :: ResourceId -> YesodDB App [Entity Tag]
getResourceTagsWithIds resId =
    select $ 
        from $ \(tag, resourceTag) -> do
        where_ (tag^.TagId ==. resourceTag^.ResourceTagTagId
            &&. resourceTag^.ResourceTagResId ==. val resId)
        return tag

-- Adjust Resource's title and type. Add all Tags to the database, collecting
-- their ids. Remove all ResourceTag relations for the Resource, and add back
-- new relations between the Resource and each Tag id collected. Possibly delete
-- old Tags if there are no other resources that share the tag.
updateResource :: ResourceId -> Text -> ResourceType -> [Tag] -> YesodDB App ()
updateResource resId title typ tags = do
    -- Adjust Resource's title and type.
    update $ \resource -> do
        set resource [ ResourceTitle =. val title
                     , ResourceType  =. val typ
                     ]
        where_ (resource^.ResourceId ==. val resId)

    -- Add all new Tags, collect their IDs, and insert ResourceTags.
    newTagIds <- mapM insertBy' tags
    mapM_ (insertUnique . ResourceTag resId) newTagIds

    -- Get all old ResourceTags, to count the number of other Resources
    -- that share the Tag (we know there's at least one, this one), and
    -- possibly delete the Tag.
    getResourceTagsByResId resId >>= mapM_ (deleteUnusedTagsAndResourceTags newTagIds)
  where
    deleteUnusedTagsAndResourceTags :: [TagId] -> Entity ResourceTag -> YesodDB App ()
    deleteUnusedTagsAndResourceTags newTagIds (Entity rtid (ResourceTag _ tid)) =
        -- Only possibly delete tags that weren't just added as this resources's tags,
        -- because our deletion criteria is that this is the only resource with the tag.
        when (tid `notElem` newTagIds) $ do
            -- Possibly delete the Tag, then unconditionally delete the ResourceTag (it's old).
            n <- P.count [ResourceTagTagId P.==. tid]
            when (n == 1) $ -- We know there's at least one.
                P.delete tid
            P.delete rtid   -- And know there's one less.
