{-# LANGUAGE ScopedTypeVariables #-}

module Model.Resource
    ( favoriteResourceDB
    , fetchAllResourcesDB
    , fetchResourceAuthorsDB
    , fetchResourceAuthorsInDB
    , fetchResourceCollectionsDB
    , fetchResourcesWithAuthorDB
    , fetchResourcesWithTagDB
    , fetchResourcesWithTypeDB
    , fetchResourceTagsDB
    , fetchResourceTypeCountsDB
    , fetchResourceTypeYearRangesDB
    , grokResourceDB
    , resourceExtension
    , unfavoriteResourceDB
    , ungrokResourceDB
    , updateResourceDB
    , updateResourceAuthorsDB
    , module Model.Resource.Internal
    ) where

import Import
import Model.Resource.Internal

import           Data.DList         (DList)
import qualified Data.DList         as DL
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Data.Time          (getCurrentTime)
import           Database.Esqueleto

-- | Grab the "important" extension of this resource (pdf, ps, etc). for
-- visual display (for instance, so mobile users don't download pdfs
-- accidentally).
resourceExtension :: Resource -> Maybe Text
resourceExtension res = case T.breakOnEnd "." (resourceUrl res) of
    (_, "pdf") -> Just "pdf"
    (_, "ps")  -> Just "ps"
    _          -> Nothing

-- | Get all resources.
fetchAllResourcesDB :: YesodDB App [Entity Resource]
fetchAllResourcesDB = selectList [] []

-- | Get the Authors of a Resource.
fetchResourceAuthorsDB :: ResourceId -> YesodDB App [Author]
fetchResourceAuthorsDB res_id = fmap (map entityVal) $
    select $
    from $ \(a `InnerJoin` ra) -> do
    on (a^.AuthorId ==. ra^.ResAuthorAuthId)
    where_ (ra^.ResAuthorResId ==. val res_id)
    orderBy [asc (ra^.ResAuthorOrd)]
    return a

-- | Get the Authors of a list of Resources, as a Map.
fetchResourceAuthorsInDB :: [ResourceId] -> YesodDB App (Map ResourceId [Author])
fetchResourceAuthorsInDB res_ids = fmap makeAuthorMap $
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

fetchResourcesWithAuthorDB :: Text -> YesodDB App [Entity Resource]
fetchResourcesWithAuthorDB name = getBy404 (UniqueAuthor name) >>= fetchResourcesWithAuthorIdDB . entityKey
  where
    fetchResourcesWithAuthorIdDB :: AuthorId -> YesodDB App [Entity Resource]
    fetchResourcesWithAuthorIdDB author_id =
        select $
        from $ \(r `InnerJoin` ra) -> do
        on (r^.ResourceId ==. ra^.ResAuthorResId)
        where_ (ra^.ResAuthorAuthId ==. val author_id)
        return r

fetchResourcesWithTagDB :: Text -> YesodDB App [Entity Resource]
fetchResourcesWithTagDB tag = getBy404 (UniqueTag tag) >>= fetchResourcesWithTagIdDB . entityKey
  where
    fetchResourcesWithTagIdDB :: TagId -> YesodDB App [Entity Resource]
    fetchResourcesWithTagIdDB tag_id =
        select $
        from $ \(r `InnerJoin` rt) -> do
        on (r^.ResourceId ==. rt^.ResourceTagResId)
        where_ (rt^.ResourceTagTagId ==. val tag_id)
        return r

fetchResourcesWithTypeDB :: ResourceType -> YesodDB App [Entity Resource]
fetchResourcesWithTypeDB res_type =
    select $
    from $ \r -> do
    where_ (r^.ResourceType ==. val res_type)
    return r

fetchResourceTagsDB :: ResourceId -> YesodDB App [Tag]
fetchResourceTagsDB res_id = fmap (map entityVal) $
    select $
    from $ \(t `InnerJoin` rt) -> do
    on (t^.TagId ==. rt^.ResourceTagTagId)
    where_ (rt^.ResourceTagResId ==. val res_id)
    orderBy [asc (t^.TagName)]
    return t

fetchResourceCollectionsDB :: ResourceId -> YesodDB App [Collection]
fetchResourceCollectionsDB res_id = fmap (map entityVal) $
    select $
    from $ \(c `InnerJoin` rc) -> do
    on (c^.CollectionId ==. rc^.ResCollectionColId)
    where_ (rc^.ResCollectionResId ==. val res_id)
    orderBy [asc (c^.CollectionName)]
    return c

favoriteResourceDB, grokResourceDB :: UserId -> ResourceId -> YesodDB App ()
favoriteResourceDB = favgrok Favorite
grokResourceDB     = favgrok Grokked

-- favgrok :: PersistEntity b => (uid -> rid -> UTCTime -> entity) -> uid -> rid -> UTCTime -> entity
favgrok constructor user_id res_id = liftIO getCurrentTime >>= void . insertUnique . constructor user_id res_id

unfavoriteResourceDB, ungrokResourceDB :: UserId -> ResourceId -> YesodDB App ()
unfavoriteResourceDB user_id = deleteBy . UniqueFavorite user_id
ungrokResourceDB     user_id = deleteBy . UniqueGrokked  user_id

-- | Update a resource.
updateResourceDB
        :: ResourceId
        -> Text           -- ^ Title
        -> [Author]
        -> Maybe Int      -- ^ Year published
        -> ResourceType
        -> [Tag]
        -> [Collection]
        -> YesodDB App ()
updateResourceDB res_id title authors published typ tags colls = do
    updateTitlePublishedType
    updateResourceAuthorsDB res_id authors
    updateTags
    updateCollections
  where
    updateTitlePublishedType =
        update $ \r -> do
        set r [ ResourceTitle     =. val title
              , ResourcePublished =. val published
              , ResourceType      =. val typ
              ]
        where_ (r^.ResourceId ==. val res_id)

    updateTags        = updateEntitiesAndRelations TagId        ResourceTag   ResourceTagResId   ResourceTagTagId   tags
    updateCollections = updateEntitiesAndRelations CollectionId ResCollection ResCollectionResId ResCollectionColId colls

    updateEntitiesAndRelations :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                                   PersistEntity relation, PersistEntityBackend relation ~ SqlBackend)
                               => EntityField entity (Key entity)
                               -> (ResourceId -> Key entity -> relation)
                               -> EntityField relation ResourceId
                               -> EntityField relation (Key entity)
                               -> [entity]
                               -> YesodDB App ()
    updateEntitiesAndRelations id_field relation relation_res_field relation_id_field vals = do
        deleteWithFkeyOnResource relation_res_field
        insertEntitiesAndRelations relation vals
        deleteUnusedEntities id_field relation_id_field

    deleteWithFkeyOnResource :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
                             => EntityField entity ResourceId
                             -> YesodDB App ()
    deleteWithFkeyOnResource fkey =
        delete $
        from $ \table ->
        where_ (table^.fkey ==. val res_id)

    insertEntitiesAndRelations :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                                   PersistEntity relation, PersistEntityBackend relation ~ SqlBackend)
                               => (ResourceId -> Key entity -> relation)
                               -> [entity]
                               -> YesodDB App ()
    insertEntitiesAndRelations entity vals =
        mapM (fmap (either entityKey id) . insertBy) vals >>= void . insertMany . map (entity res_id)

    deleteUnusedEntities :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                             PersistEntity relation, PersistEntityBackend relation ~ SqlBackend)
                         => EntityField entity (Key entity)
                         -> EntityField relation (Key entity)
                         -> YesodDB App ()
    deleteUnusedEntities id_field relation_id_field =
        delete $
        from $ \table ->
        where_ (table^.id_field `notIn` (subList_selectDistinct $
                                         from $ \relation_table ->
                                         return (relation_table^.relation_id_field)))

updateResourceAuthorsDB :: ResourceId -> [Author] -> YesodDB App ()
updateResourceAuthorsDB res_id authors = do
    deleteResAuthors
    insertAuthors >>= insertResAuthors
    deleteUnusedAuthors
  where
    deleteResAuthors =
        delete $
        from $ \ra ->
        where_ (ra^.ResAuthorResId ==. val res_id)

    insertAuthors :: YesodDB App [AuthorId]
    insertAuthors = mapM (fmap (either entityKey id) . insertBy) authors

    insertResAuthors :: [AuthorId] -> YesodDB App ()
    insertResAuthors = void . insertMany . map (\(n,auth_id) -> ResAuthor res_id auth_id n) . zip [0..]

    deleteUnusedAuthors =
        delete $
        from $ \a ->
        where_ (a^.AuthorId `notIn` (subList_selectDistinct $
                                     from $ \ra ->
                                     return (ra^.ResAuthorAuthId)))

-- | Get a map of ResourceType to the number of Resources with that type.
fetchResourceTypeCountsDB :: YesodDB App (Map ResourceType Int)
fetchResourceTypeCountsDB = fmap (M.fromList . map fromValue) $
    select $
    from $ \r -> do
    groupBy (r^.ResourceType)
    return (r^.ResourceType :: SqlExpr (Value ResourceType), countRows :: SqlExpr (Value Int))

-- | Get the year range of all ResourceTypes. If none of the a ResourceType's
-- Resources have any published year, then the ResourceType will not exist in
-- the returned map.
fetchResourceTypeYearRangesDB :: YesodDB App (Map ResourceType (Int, Int))
fetchResourceTypeYearRangesDB = fmap (foldr f mempty) $
    select $
    from $ \r -> do
    groupBy (r^.ResourceType)
    return (r^.ResourceType, min_ (r^.ResourcePublished), max_ (r^.ResourcePublished))
  where
    f :: (Value ResourceType, Value (Maybe (Maybe Int)), Value (Maybe (Maybe Int)))
      -> Map ResourceType (Int, Int)
      -> Map ResourceType (Int, Int)
    f (Value _,        Value (Just Nothing),  Value (Just Nothing))  = id
    f (Value res_type, Value (Just (Just m)), Value Nothing)         = M.insert res_type (m, m)
    f (Value res_type, Value Nothing,         Value (Just (Just m))) = M.insert res_type (m, m)
    f (Value res_type, Value (Just (Just m)), Value (Just (Just n))) = M.insert res_type (m, n)
    f (_,              Value Nothing,         Value Nothing)         = id
    -- How could min_ return NULL but max not, or vice versa?
    f (_, _, _) = error "fetchResourceTypeYearRangesDB: incorrect assumption about return value of min_/max_"
