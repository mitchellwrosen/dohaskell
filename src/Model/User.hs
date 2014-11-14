module Model.User
    ( fetchGrokkedCountsByAuthorDB
    , fetchGrokkedCountsByCollectionDB
    , fetchGrokkedCountsByTagDB
    , fetchGrokkedCountsByTypeDB
    , fetchGrokkedResourceIdsInDB
    , fetchNumSubmittedResourcesDB
    , fetchNumGrokkedResourcesDB
    , fetchSubmittedResourcesDB
    , fetchUsersInDB
    , isAdministratorDB
    , thisUserHasAuthorityOverDB
    , updateUserDisplayNameDB
    , userHasAuthorityOverDB
    ) where

import Import

import           Model.List
import           Model.Resource

import           Database.Esqueleto
import qualified Data.Map           as M

-- | Get the number of Resources this User has grokked, grouped by Author/Collection/Tag/Type.
fetchGrokkedCountsByAuthorDB     :: UserId -> YesodDB App (Map AuthorId Int)
fetchGrokkedCountsByCollectionDB :: UserId -> YesodDB App (Map CollectionId Int)
fetchGrokkedCountsByTagDB        :: UserId -> YesodDB App (Map TagId Int)
fetchGrokkedCountsByTypeDB       :: UserId -> YesodDB App (Map ResourceType Int)
fetchGrokkedCountsByAuthorDB     = fetchGrokkedCountsByFieldDB ResAuthorResId     ResAuthorAuthId
fetchGrokkedCountsByCollectionDB = fetchGrokkedCountsByFieldDB ResCollectionResId ResCollectionColId
fetchGrokkedCountsByTagDB        = fetchGrokkedCountsByFieldDB ResourceTagResId   ResourceTagTagId
fetchGrokkedCountsByTypeDB       = fetchGrokkedCountsByFieldDB ResourceId         ResourceType

fetchGrokkedCountsByFieldDB :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend,
                                PersistField key, Ord key)
                            => EntityField entity ResourceId
                            -> EntityField entity key
                            -> UserId
                            -> YesodDB App (Map key Int)
fetchGrokkedCountsByFieldDB res_id_field key user_id =
    fetchGrokkedListIdDB >>= \case
        Nothing -> return mempty
        Just grokked_list_id -> fmap (M.fromList . map fromValue) $
            select $
            from $ \(table `InnerJoin` li) -> do
            on (table^.res_id_field ==. li^.ListItemResId)
            where_ $
                li^.ListItemListId ==. val grokked_list_id &&.
                li^.ListItemUserId ==. val user_id
            groupBy (table^.key)
            return (table^.key, countRows :: SqlExpr (Value Int))

-- | Given a list of Resources, return only those grokked by the given User.
fetchGrokkedResourceIdsInDB :: UserId -> [ResourceId] -> YesodDB App [ResourceId]
fetchGrokkedResourceIdsInDB user_id res_ids =
    fetchGrokkedListIdDB >>= \case
        Nothing -> return []
        Just grokked_list_id -> fmap (map unValue) $
            select $
            from $ \li -> do
            where_ $
                li^.ListItemListId ==. val grokked_list_id &&.
                li^.ListItemUserId ==. val user_id         &&.
                li^.ListItemResId `in_` valList res_ids
            return (li^.ListItemResId)

fetchSubmittedResourcesDB :: UserId -> YesodDB App [Entity Resource]
fetchSubmittedResourcesDB uid =
    select $
    from $ \(u `InnerJoin` r) -> do
    on (u^.UserId ==. r^.ResourceUserId)
    where_ (u^.UserId ==. val uid)
    return r

fetchNumSubmittedResourcesDB :: UserId -> YesodDB App Int
fetchNumSubmittedResourcesDB user_id = fmap (\[Value n] -> n) $
    select $
    from $ \(u `InnerJoin` r) -> do
    on (u^.UserId ==. r^.ResourceUserId)
    where_ (u^.UserId ==. val user_id)
    return countRows

fetchNumGrokkedResourcesDB :: UserId -> YesodDB App Int
fetchNumGrokkedResourcesDB user_id = fetchGrokkedListIdDB >>= \case
    Nothing -> return 0
    Just grokked_list_id -> fmap (\[Value n] -> n) $
        select $
        from $ \li -> do
        where_ $
            li^.ListItemListId ==. val grokked_list_id &&.
            li^.ListItemUserId ==. val user_id
        return countRows

-- | Fetch all Users in the given list of ids.
fetchUsersInDB :: [UserId] -> YesodDB App [Entity User]
fetchUsersInDB user_ids =
    select $
    from $ \u -> do
    where_ (u^.UserId `in_` valList user_ids)
    return u

isAdministratorDB :: UserId -> YesodDB App Bool
isAdministratorDB = fmap (maybe False userIsAdministrator) . get

updateUserDisplayNameDB :: UserId -> Text -> YesodDB App ()
updateUserDisplayNameDB uid displayName =
    update $ \u -> do
    set u [UserDisplayName =. val displayName]
    where_ (u^.UserId ==. val uid)

-- 'bully' has authority over 'nerd' if 'bully' is an administrator,
-- or if 'bully' and 'nerd' are the same user.
--
-- Assumes that 'bully' is an actual user id, not from a URL.
userHasAuthorityOverDB :: UserId -> UserId -> YesodDB App Bool
userHasAuthorityOverDB bully nerd = do
    isAdmin <- userIsAdministrator <$> getJust bully
    return $
        if isAdmin
            then True
            else (bully == nerd)

-- | Like userHasAuthorityOverDB, but uses the current user ('this' user) as the
-- first argument.
thisUserHasAuthorityOverDB :: UserId -> Handler Bool
thisUserHasAuthorityOverDB nerd = maybeAuthId >>= \case
    Nothing    -> return False
    Just bully -> runDB (userHasAuthorityOverDB bully nerd)
