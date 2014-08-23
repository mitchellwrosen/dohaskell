module Model.User
    ( fetchFavoriteResourcesDB
    , fetchFavoriteResourceIdsDB
    , fetchFavoriteResourceIdsInDB
    , fetchGrokkedCountsByAuthorDB
    , fetchGrokkedCountsByTagDB
    , fetchGrokkedCountsByTypeDB
    , fetchGrokkedResourcesDB
    , fetchGrokkedResourceIdsDB
    , fetchGrokkedResourceIdsInDB
    , fetchNumFavoriteResourcesDB
    , fetchNumGrokkedResourcesDB
    , fetchNumSubmittedResourcesDB
    , fetchSubmittedResourcesDB
    , isAdministratorDB
    , thisUserHasAuthorityOverDB
    , updateUserDisplayNameDB
    , userHasAuthorityOverDB
    ) where

import Import

import           Model.Resource

import           Database.Esqueleto
import qualified Data.Map           as M

fetchFavoriteResourcesDB, fetchGrokkedResourcesDB :: UserId -> YesodDB App [Entity Resource]

fetchFavoriteResourcesDB user_id =
    select $
    from $ \r -> do
    where_ (r^.ResourceId `in_` (subList_select $
                                 from $ \f -> do
                                 where_ (f^.FavoriteUserId ==. val user_id)
                                 return (f^.FavoriteResId)))
    return r

fetchGrokkedResourcesDB user_id =
    select $
    from $ \r -> do
    where_ (r^.ResourceId `in_` (subList_select $
                                 from $ \g -> do
                                 where_ (g^.GrokkedUserId ==. val user_id)
                                 return (g^.GrokkedResId)))
    return r

fetchFavoriteResourceIdsDB, fetchGrokkedResourceIdsDB :: UserId -> YesodDB App [ResourceId]
fetchFavoriteResourceIdsDB uid = fmap (map unValue) $
    select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val uid)
    return (f^.FavoriteResId)
fetchGrokkedResourceIdsDB uid = fmap (map unValue) $
    select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val uid)
    return (g^.GrokkedResId)

fetchFavoriteResourceIdsInDB :: [ResourceId] -> UserId -> YesodDB App [ResourceId]
fetchGrokkedResourceIdsInDB  :: [ResourceId] -> UserId -> YesodDB App [ResourceId]

fetchFavoriteResourceIdsInDB resourceIds userId = fmap (map unValue) $
    select $
    from $ \f -> do
    where_ $
        f^.FavoriteUserId ==. val userId &&.
        f^.FavoriteResId `in_` valList resourceIds
    return (f^.FavoriteResId)

fetchGrokkedResourceIdsInDB resourceIds userId = fmap (map unValue) $
    select $
    from $ \g -> do
    where_ $
        g^.GrokkedUserId ==. val userId &&.
        g^.GrokkedResId `in_` valList resourceIds
    return (g^.GrokkedResId)

-- | Get the number of Resources this User has grokked, grouped by Author.
fetchGrokkedCountsByAuthorDB :: UserId -> YesodDB App (Map AuthorId Int)
fetchGrokkedCountsByAuthorDB user_id = fmap (M.fromList . map fromValue) $
    select $
    from $ \(g `InnerJoin` ra) -> do
    on (g^.GrokkedResId ==. ra^.ResAuthorResId)
    where_ (g^.GrokkedUserId ==. val user_id)
    groupBy (ra^.ResAuthorAuthId)
    return (ra^.ResAuthorAuthId, countRows :: SqlExpr (Value Int))

-- | Get the number of Resources this User has grokked, grouped by Tag.
fetchGrokkedCountsByTagDB :: UserId -> YesodDB App (Map TagId Int)
fetchGrokkedCountsByTagDB user_id = fmap (M.fromList . map fromValue) $
    select $
    from $ \(g `InnerJoin` rt) -> do
    on (g^.GrokkedResId ==. rt^.ResourceTagResId)
    where_ (g^.GrokkedUserId ==. val user_id)
    groupBy (rt^.ResourceTagTagId)
    return (rt^.ResourceTagTagId, countRows :: SqlExpr (Value Int))

-- | Get the number of Resources this User has grokked, grouped by ResourceType.
fetchGrokkedCountsByTypeDB :: UserId -> YesodDB App (Map ResourceType Int)
fetchGrokkedCountsByTypeDB user_id = fmap (M.fromList . map fromValue) $
    select $
    from $ \(g `InnerJoin` r) -> do
    on (g^.GrokkedResId ==. r^.ResourceId)
    where_ (g^.GrokkedUserId ==. val user_id)
    groupBy (r^.ResourceType)
    return (r^.ResourceType, countRows :: SqlExpr (Value Int))

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

fetchNumFavoriteResourcesDB :: UserId -> YesodDB App Int
fetchNumFavoriteResourcesDB user_id = fmap (\[Value n] -> n) $
    select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val user_id)
    return countRows

fetchNumGrokkedResourcesDB :: UserId -> YesodDB App Int
fetchNumGrokkedResourcesDB user_id = fmap (\[Value n] -> n) $
    select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val user_id)
    return countRows

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

-- Like userHasAuthorityOverDB, but uses the current user
-- ('this' user) as the first argument.
thisUserHasAuthorityOverDB :: UserId -> Handler Bool
thisUserHasAuthorityOverDB nerd = maybeAuthId >>= \case
    Nothing    -> return False
    Just bully -> runDB (userHasAuthorityOverDB bully nerd)
