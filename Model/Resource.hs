module Model.Resource
    ( deleteResource
    , getFavoriteResources
    , getGrokkedCounts
    , getGrokkedResources
    , getResourceTags
    , getResourceTagsWithIds
    , getResourcesWithTag
    , updateResource
    ) where

import Import

import qualified Data.Map             as M
import qualified Data.Set             as S
import           Database.Esqueleto
import qualified Database.Persist   as P

import Database.Persist.Class.Extra (insertBy')
import Model.ResourceTag            (getResourceTagsByResId)

deleteResource :: ResourceId -> YesodDB App ()
deleteResource resId = do
    tagIds <- map (resourceTagTagId . entityVal)  <$> getResourceTagsByResId resId
    mapM_ deleteUnusedTag tagIds
    deleteCascade resId
  where
    deleteUnusedTag :: TagId -> YesodDB App ()
    deleteUnusedTag tid = do
        [n] <- fmap (map unValue) $
            select $
                from $ \rt -> do
                where_ (rt^.ResourceTagTagId ==. val tid)
                return countRows
        -- resource hasn't been deleted yet, so compare to 1
        when (n == (1::Int)) $
            deleteKey tid

-- TODO: combine this with getGrokkedResources
getFavoriteResources :: UserId -> YesodDB App (Set ResourceId)
getFavoriteResources uid = foldr (\v s -> s <> S.singleton (unValue v)) mempty <$>
  (select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val uid)
    return (f^.FavoriteResId))

getGrokkedResources :: UserId -> YesodDB App (Set ResourceId)
getGrokkedResources uid = foldr (\v s -> s <> S.singleton (unValue v)) mempty <$>
  (select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val uid)
    return (g^.GrokkedResId))

-- Get a the number of resources this user has grokked, grouped by tag.
getGrokkedCounts :: UserId -> YesodDB App (Map TagId Int)
getGrokkedCounts uid = valsToMap <$>
    (select $
        from $ \(g `InnerJoin` rt) -> do
        on (g^.GrokkedUserId ==. val uid &&.
            g^.GrokkedResId ==. rt^.ResourceTagResId)
        groupBy (rt^.ResourceTagTagId)
        return (rt^.ResourceTagTagId, countRows))
  where
    valsToMap :: [(Value TagId, Value Int)] -> Map TagId Int
    valsToMap = foldr step mempty
      where
        step (Value tagId, Value n) = M.insert tagId n

getResourceTags :: ResourceId -> YesodDB App [Tag]
getResourceTags = fmap (map entityVal) . getResourceTagsWithIds

-- Longer name because it's probably more likely that we don't care about the
-- tags' ids.
getResourceTagsWithIds :: ResourceId -> YesodDB App [Entity Tag]
getResourceTagsWithIds resId =
    select $
        from $ \(t, rt) -> do
        where_ (t^.TagId ==. rt^.ResourceTagTagId &&.
                rt^.ResourceTagResId ==. val resId)
        orderBy [asc (t^.TagText)]
        return t

getResourcesWithTag :: Text -> YesodDB App [Entity Resource]
getResourcesWithTag tag = getBy404 (UniqueTagText tag) >>= getResourcesWithTagId . entityKey
  where
    getResourcesWithTagId :: TagId -> YesodDB App [Entity Resource]
    getResourcesWithTagId tagId =
        select $
            from $ \(r, rt) -> do
            where_ (r^.ResourceId ==. rt^.ResourceTagResId &&.
                    rt^.ResourceTagTagId ==. val tagId)
            return r

-- Adjust Resource's title, author, published, and type. Add all Tags to the database,
-- collecting their ids. Remove all ResourceTag relations for the Resource, and add back
-- new relations between the Resource and each Tag id collected. Possibly delete
-- old Tags if there are no other resources that share the tag.
updateResource :: ResourceId -> Text -> Maybe Text -> Maybe Int -> ResourceType -> [Tag] -> YesodDB App ()
updateResource resId title author published typ tags = do
    -- Adjust Resource's title and type.
    update $ \resource -> do
        set resource [ ResourceTitle     =. val title
                     , ResourceAuthor    =. val author
                     , ResourcePublished =. val published
                     , ResourceType      =. val typ
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
                deleteKey tid
            deleteKey rtid   -- And know there's one less.
