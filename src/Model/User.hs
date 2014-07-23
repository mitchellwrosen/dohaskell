module Model.User
    ( getFavoriteResources
    , getFavoriteResourceIds
    , getFavoriteResourceIdsIn
    , getGrokkedCountsByAuthor
    , getGrokkedCountsByTag
    , getGrokkedCountsByType
    , getGrokkedResources
    , getGrokkedResourceIds
    , getGrokkedResourceIdsIn
    , getNumFavoriteResources
    , getNumGrokkedResources
    , getNumSubmittedResources
    , getSubmittedResources
    , isAdministrator
    , thisUserHasAuthorityOver
    , updateUserDisplayName
    , userHasAuthorityOver
    ) where

import Import

import           Model.Resource

import           Database.Esqueleto
import qualified Data.Map           as M

getFavoriteResources, getGrokkedResources :: UserId -> YesodDB App [Entity Resource]
getFavoriteResources user_id =
    select $
    from $ \r -> do
    where_ (r^.ResourceId `in_`
        (subList_select $
         from $ \f -> do
         where_ (f^.FavoriteUserId ==. val user_id)
         return (f^.FavoriteResId)))
    return r
getGrokkedResources user_id =
    select $
    from $ \r -> do
    where_ (r^.ResourceId `in_`
        (subList_select $
         from $ \g -> do
         where_ (g^.GrokkedUserId ==. val user_id)
         return (g^.GrokkedResId)))
    return r

getFavoriteResourceIds, getGrokkedResourceIds :: UserId -> YesodDB App [ResourceId]
getFavoriteResourceIds uid = fmap (map unValue) $
    select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val uid)
    return (f^.FavoriteResId)
getGrokkedResourceIds uid = fmap (map unValue) $
    select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val uid)
    return (g^.GrokkedResId)

getFavoriteResourceIdsIn :: [ResourceId] -> UserId -> YesodDB App [ResourceId]
getGrokkedResourceIdsIn  :: [ResourceId] -> UserId -> YesodDB App [ResourceId]

getFavoriteResourceIdsIn resourceIds userId = fmap (map unValue) $
    select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val userId &&.
            f^.FavoriteResId `in_` valList resourceIds)
    return (f^.FavoriteResId)

getGrokkedResourceIdsIn resourceIds userId = fmap (map unValue) $
    select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val userId &&.
            g^.GrokkedResId `in_` valList resourceIds)
    return (g^.GrokkedResId)

-- | Get the number of Resources this User has grokked, grouped by Author.
getGrokkedCountsByAuthor :: UserId -> YesodDB App (Map AuthorId Int)
getGrokkedCountsByAuthor user_id = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value AuthorId, Value Int)]
    sel = select $
          from $ \(g `InnerJoin` ra) -> do
          on (g^.GrokkedResId ==. ra^.ResAuthorResId)
          where_ (g^.GrokkedUserId ==. val user_id)
          groupBy (ra^.ResAuthorAuthId)
          return (ra^.ResAuthorAuthId, countRows)

-- | Get the number of Resources this User has grokked, grouped by Tag.
getGrokkedCountsByTag :: UserId -> YesodDB App (Map TagId Int)
getGrokkedCountsByTag user_id = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value TagId, Value Int)]
    sel = select $
          from $ \(g `InnerJoin` rt) -> do
          on (g^.GrokkedResId ==. rt^.ResourceTagResId)
          where_ (g^.GrokkedUserId ==. val user_id)
          groupBy (rt^.ResourceTagTagId)
          return (rt^.ResourceTagTagId, countRows)

-- | Get the number of Resources this User has grokked, grouped by ResourceType.
getGrokkedCountsByType :: UserId -> YesodDB App (Map ResourceType Int)
getGrokkedCountsByType user_id = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value ResourceType, Value Int)]
    sel = select $
          from $ \(g `InnerJoin` r) -> do
          on (g^.GrokkedResId ==. r^.ResourceId)
          where_ (g^.GrokkedUserId ==. val user_id)
          groupBy (r^.ResourceType)
          return (r^.ResourceType, countRows)

getSubmittedResources :: UserId -> YesodDB App [Entity Resource]
getSubmittedResources uid =
    select $
    from $ \(u `InnerJoin` r) -> do
    on (u^.UserId ==. r^.ResourceUserId)
    where_ (u^.UserId ==. val uid)
    return r

getNumSubmittedResources :: UserId -> YesodDB App Int
getNumSubmittedResources user_id = fmap (\[Value n] -> n) $
    select $
    from $ \(u `InnerJoin` r) -> do
    on (u^.UserId ==. r^.ResourceUserId)
    where_ (u^.UserId ==. val user_id)
    return countRows

getNumFavoriteResources :: UserId -> YesodDB App Int
getNumFavoriteResources user_id = fmap (\[Value n] -> n) $
    select $
    from $ \f -> do
    where_ (f^.FavoriteUserId ==. val user_id)
    return countRows

getNumGrokkedResources :: UserId -> YesodDB App Int
getNumGrokkedResources user_id = fmap (\[Value n] -> n) $
    select $
    from $ \g -> do
    where_ (g^.GrokkedUserId ==. val user_id)
    return countRows

isAdministrator :: UserId -> YesodDB App Bool
isAdministrator = fmap (maybe False userIsAdministrator) . get

updateUserDisplayName :: UserId -> Text -> YesodDB App ()
updateUserDisplayName uid displayName =
    update $ \u -> do
    set u [UserDisplayName =. val displayName]
    where_ (u^.UserId ==. val uid)

-- 'bully' has authority over 'nerd' if 'bully' is an administrator,
-- or of 'bully' and 'nerd' are the same user.
--
-- Assumes that 'bully' is an actual user id, not from a URL.
userHasAuthorityOver :: UserId -> UserId -> YesodDB App Bool
userHasAuthorityOver bully nerd = do
    isAdmin <- userIsAdministrator <$> getJust bully
    return $
        if isAdmin
            then True
            else (bully == nerd)

-- Like userHasAuthorityOver, but uses the current user ('this' user) as the
-- first argument.
thisUserHasAuthorityOver :: UserId -> Handler Bool
thisUserHasAuthorityOver nerd = maybeAuthId >>= \case
    Nothing    -> return False
    Just bully -> runDB $ userHasAuthorityOver bully nerd
