{-# LANGUAGE ScopedTypeVariables #-}

module Model.Resource
    ( fetchAllResourcesDB
    , fetchResourceAuthorsDB
    , fetchResourceAuthorsInDB
    , fetchResourceCollectionsDB
    , fetchResourceCommentsDB
    , fetchResourceFieldCountsDB
    , fetchResourceFieldYearRangesDB
    , fetchResourceGrokkedCountsInDB
    , fetchResourcesByAuthorDB
    , fetchResourcesInCollectionDB
    , fetchResourcesWithTagDB
    , fetchResourcesWithTypeDB
    , fetchResourceTagsDB
    , fetchResourceTypeCountsDB
    , fetchResourceTypeYearRangesDB
    , insertResourceCommentDB
    , resourceExtension
    , updateResourceDB
    , updateResourceAuthorsDB
    , module Model.Resource.Internal
    ) where

import Import
import Model.Resource.Internal

import           Model.List

import           Data.DList         (DList)
import qualified Data.DList         as DL
import qualified Data.Map           as M
import qualified Data.Text          as T
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

fetchResourcesByAuthorDB :: Text -> YesodDB App [Entity Resource]
fetchResourcesByAuthorDB = fetchResourcesWithFieldDB UniqueAuthor ResAuthorResId ResAuthorAuthId

fetchResourcesInCollectionDB :: Text -> YesodDB App [Entity Resource]
fetchResourcesInCollectionDB = fetchResourcesWithFieldDB UniqueCollection ResCollectionResId ResCollectionColId

fetchResourcesWithTagDB :: Text -> YesodDB App [Entity Resource]
fetchResourcesWithTagDB = fetchResourcesWithFieldDB UniqueTag ResourceTagResId ResourceTagTagId

-- | Abstract fetching all Resources with a particular Text field (Author/Collection/Tag).
fetchResourcesWithFieldDB :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                              PersistEntity relation, PersistEntityBackend relation ~ SqlBackend)
                          => (Text -> Unique entity)
                          -> EntityField relation ResourceId
                          -> EntityField relation (Key entity)
                          -> Text
                          -> YesodDB App [Entity Resource]
fetchResourcesWithFieldDB unique_entity res_id_field key_field name = getBy (unique_entity name) >>= \case
    Nothing -> return []
    Just (Entity key _) ->
        select $
        from $ \(r `InnerJoin` table) -> do
        on (r^.ResourceId ==. table^.res_id_field)
        where_ (table^.key_field ==. val key)
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
fetchResourceTypeCountsDB = fetchResourceFieldCountsDB ResourceType

fetchResourceFieldCountsDB :: forall entity key.
                              (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                               PersistField key, Ord key)
                           => EntityField entity key -> YesodDB App (Map key Int)
fetchResourceFieldCountsDB key = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value key, Value Int)]
    sel = select $
          from $ \table -> do
          groupBy (table^.key)
          return (table^.key, countRows)

fetchResourceTypeYearRangesDB :: YesodDB App (Map ResourceType (Int, Int))
fetchResourceTypeYearRangesDB = fmap (foldr mkYearMap mempty) $
    select $
    from $ \r -> do
    groupBy (r^.ResourceType)
    return (r^.ResourceType, min_ (r^.ResourcePublished), max_ (r^.ResourcePublished))

fetchResourceFieldYearRangesDB
        :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, PersistField field, Ord field)
        => EntityField entity ResourceId
        -> EntityField entity field -> YesodDB App (Map field (Int, Int))
fetchResourceFieldYearRangesDB res_id_field field = fmap (foldr mkYearMap mempty) $
    select $
    from $ \(r `InnerJoin` table) -> do
    on (r^.ResourceId ==. table^.res_id_field)
    groupBy (table^.field)
    return (table^.field, min_ (r^.ResourcePublished), max_ (r^.ResourcePublished))

mkYearMap :: Ord v
          => (Value v, Value (Maybe (Maybe Int)), Value (Maybe (Maybe Int)))
          -> Map v (Int, Int)
          -> Map v (Int, Int)
mkYearMap (Value _, Value (Just Nothing),  Value (Just Nothing))  = id
mkYearMap (Value v, Value (Just (Just m)), Value Nothing)         = M.insert v (m, m)
mkYearMap (Value v, Value Nothing,         Value (Just (Just m))) = M.insert v (m, m)
mkYearMap (Value v, Value (Just (Just m)), Value (Just (Just n))) = M.insert v (m, n)
mkYearMap (_,       Value Nothing,         Value Nothing)         = id
-- How could min_ return NULL but max not, or vice versa?
mkYearMap (_, _, _) = error "fetchResourceFieldYearRangesDB: incorrect assumption about return value of min_/max_"

-- | Given a list of Resources, get how many times each has been grokked.
fetchResourceGrokkedCountsInDB :: [ResourceId] -> YesodDB App (Map ResourceId Int)
fetchResourceGrokkedCountsInDB res_ids =
    fetchGrokkedListIdDB >>= \case
        Nothing -> return mempty -- no one has grokked anything
        Just grokked_list_id -> fmap (foldr go mempty) $
            select $
            from $ \li -> do
            where_ $
                li^.ListItemListId ==. val grokked_list_id &&.
                li^.ListItemResId `in_` valList res_ids
            groupBy (li^.ListItemResId)
            return (li^.ListItemResId, countRows)
  where
    go :: (Value ResourceId, Value Int) -> Map ResourceId Int -> Map ResourceId Int
    go (Value res_id, Value n) = M.insert res_id n

-- | Fetch all comments on a Resource, ordered by ascending CommentId (because
-- comment ids are monotonically increasing).
fetchResourceCommentsDB :: ResourceId -> YesodDB App [Entity Comment]
fetchResourceCommentsDB res_id =
    select $
    from $ \c -> do
    where_ (c^.CommentResId ==. val res_id)
    orderBy [asc (c^.CommentId)]
    return c

insertResourceCommentDB :: Comment -> YesodDB App ()
insertResourceCommentDB = insert_
