{-# LANGUAGE ScopedTypeVariables #-}

module Model.Resource
    ( deleteResource
    , favoriteResource
    , getAllResources
    , getAuthorNames
    , getAuthors
    , getAuthorsIn
    , getResourcesWithAuthor
    , getResourcesWithTag
    , getTags
    , getTagEntities
    , grokResource
    , unfavoriteResource
    , ungrokResource
    , updateResource
    , updateResourceAuthors
    , module Model.Resource.Internal
    ) where

import Import
import Model.Resource.Internal

import           Model.Utils        (getAllEntities)
import           Handler.Utils      (alphabeticIgnoreCase)

import           Data.DList         (DList)
import qualified Data.DList         as DL
import qualified Data.Map           as M
import qualified Data.Set           as S
import           Database.Esqueleto

-- | Get all resources, sorted alphabetically (ignore case).
getAllResources :: YesodDB App [Entity Resource]
getAllResources = getAllEntities (alphabeticIgnoreCase resourceTitle)

-- | Delete a Resource, its now-unused Tags, and anything with a foreign key on its ID.
deleteResource :: ResourceId -> YesodDB App ()
deleteResource res_id = do
    getResourceTags res_id >>= mapM_ deleteUnusedTag . map (resourceTagTagId . entityVal)
    deleteCascade res_id
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

-- | Get the Authors of a Resource.
getAuthors :: ResourceId -> YesodDB App [Author]
getAuthors res_id = fmap (map entityVal) $
    select $
    from $ \(a `InnerJoin` ra) -> do
    on (a^.AuthorId ==. ra^.ResAuthorAuthId)
    where_ (ra^.ResAuthorResId ==. val res_id)
    orderBy [asc (ra^.ResAuthorOrd)]
    return a

getAuthorNames :: ResourceId -> YesodDB App [Text]
getAuthorNames = fmap (map authorName) . getAuthors

-- | Get the Authors of a list of Resources, as a Map.
getAuthorsIn :: [ResourceId] -> YesodDB App (Map ResourceId [Author])
getAuthorsIn res_ids = fmap makeAuthorMap $
    select $
    from $ \(a `InnerJoin` ra) -> do
    on (a^.AuthorId ==. ra^.ResAuthorAuthId)
    where_ (ra^.ResAuthorResId `in_` valList res_ids)
    orderBy [asc (ra^.ResAuthorOrd)]
    return (ra^.ResAuthorResId, a)
  where
    makeAuthorMap :: [(Value ResourceId, Entity Author)] -> Map ResourceId [Author]
    makeAuthorMap = fmap DL.toList . foldr step mempty
      where
        step :: (Value ResourceId, Entity Author)
             -> Map ResourceId (DList Author)
             -> Map ResourceId (DList Author)
        step (Value res_id, Entity _ author) = M.insertWith (<>) res_id (DL.singleton author)

getResourceTags :: ResourceId -> YesodDB App [Entity ResourceTag]
getResourceTags resId =
    select $
    from $ \rt -> do
    where_ (rt^.ResourceTagResId ==. val resId)
    return rt

getResourcesWithAuthor :: Text -> YesodDB App [Entity Resource]
getResourcesWithAuthor name = getBy404 (UniqueAuthor name) >>= getResourcesWithAuthorId . entityKey
  where
    getResourcesWithAuthorId :: AuthorId -> YesodDB App [Entity Resource]
    getResourcesWithAuthorId author_id =
        select $
        from $ \(r `InnerJoin` ra) -> do
        on (r^.ResourceId ==. ra^.ResAuthorResId)
        where_ (ra^.ResAuthorAuthId ==. val author_id)
        return r

getResourcesWithTag :: Text -> YesodDB App [Entity Resource]
getResourcesWithTag tag = getBy404 (UniqueTag tag) >>= getResourcesWithTagId . entityKey
  where
    getResourcesWithTagId :: TagId -> YesodDB App [Entity Resource]
    getResourcesWithTagId tagId =
        select $
        from $ \(r `InnerJoin` rt) -> do
        on (r^.ResourceId ==. rt^.ResourceTagResId)
        where_ (rt^.ResourceTagTagId ==. val tagId)
        return r

getTags :: ResourceId -> YesodDB App [Text]
getTags res_id = fmap (map unValue) $
    select $
    from $ \(t `InnerJoin` rt) -> do
    on (t^.TagId ==. rt^.ResourceTagTagId)
    where_ (rt^.ResourceTagResId ==. val res_id)
    orderBy [asc (t^.TagTag)]
    return (t^.TagTag)

getTagEntities :: ResourceId -> YesodDB App [Entity Tag]
getTagEntities res_id =
    select $
    from $ \(t `InnerJoin` rt) -> do
    on (t^.TagId ==. rt^.ResourceTagTagId)
    where_ (rt^.ResourceTagResId ==. val res_id)
    orderBy [asc (t^.TagTag)]
    return t

favoriteResource, grokResource :: UserId -> ResourceId -> YesodDB App ()
favoriteResource user_id = void . insertUnique . Favorite user_id
grokResource     user_id = void . insertUnique . Grokked  user_id

unfavoriteResource :: UserId -> ResourceId -> YesodDB App ()
unfavoriteResource user_id = deleteBy . UniqueFavorite user_id
ungrokResource     user_id = deleteBy . UniqueGrokked  user_id

-- | Update a resource.
updateResource :: ResourceId     -- ^ ID.
               -> Text           -- ^ Title.
               -> [Author]       -- ^ Authors.
               -> Maybe Int      -- ^ Published.
               -> ResourceType   -- ^ Type.
               -> [Tag]          -- ^ Tags.
               -> YesodDB App ()
updateResource res_id title authors published typ tags = do
    updateResourceTitlePublishedType
    updateResourceTags
    updateResourceAuthors res_id authors
  where
    updateResourceTitlePublishedType = do
        update $ \r -> do
        set r [ ResourceTitle     =. val title
              , ResourcePublished =. val published
              , ResourceType      =. val typ
              ]
        where_ (r^.ResourceId ==. val res_id)

    updateResourceTags = do
        deleteResourceTags
        insertTags >>= insertResourceTags
        deleteUnusedTags
      where
        deleteResourceTags =
            delete $
            from $ \rt -> do
            where_ (rt^.ResourceTagResId ==. val res_id)

        insertTags :: YesodDB App [TagId]
        insertTags = mapM (fmap (either entityKey id) . insertBy) tags

        insertResourceTags :: [TagId] -> YesodDB App ()
        insertResourceTags = void . insertMany . map (ResourceTag res_id)

        deleteUnusedTags =
            delete $
            from $ \t -> do
            where_ (t^.TagId `notIn` (subList_selectDistinct $
                                      from $ \rt -> do
                                      return (rt^.ResourceTagTagId)))

updateResourceAuthors :: ResourceId -> [Author] -> YesodDB App ()
updateResourceAuthors res_id authors = do
    deleteResAuthors
    insertAuthors >>= insertResAuthors
    deleteUnusedAuthors
  where
    deleteResAuthors =
        delete $
        from $ \ra -> do
        where_ (ra^.ResAuthorResId ==. val res_id)

    insertAuthors :: YesodDB App [AuthorId]
    insertAuthors = mapM (fmap (either entityKey id) . insertBy) authors

    insertResAuthors :: [AuthorId] -> YesodDB App ()
    insertResAuthors = void . insertMany . map (\(n,auth_id) -> ResAuthor res_id auth_id n) . zip [0..]

    deleteUnusedAuthors =
        delete $
        from $ \a -> do
        where_ (a^.AuthorId `notIn` (subList_selectDistinct $
                                     from $ \ra -> do
                                     return (ra^.ResAuthorAuthId)))
