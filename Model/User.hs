module Model.User
    ( getFavoriteResources
    , getFavoriteResourcesIn
    , getGrokkedCounts
    , getGrokkedResources
    , getGrokkedResourcesIn
    , getPostedResources
    , isAdministrator
    , thisUserHasAuthorityOver
    , updateUserDisplayName
    , userHasAuthorityOver
    ) where

import Import

import           Database.Esqueleto
import qualified Data.Map           as M
import qualified Data.Set           as S

-- TODO: combine this with getGrokkedResources
getFavoriteResources :: UserId -> YesodDB App (Set ResourceId)
getGrokkedResources  :: UserId -> YesodDB App (Set ResourceId)

getFavoriteResources uid = fmap (S.fromList . map unValue) $
    select $
        from $ \f -> do
        where_ (f^.FavoriteUserId ==. val uid)
        return (f^.FavoriteResId)

getGrokkedResources uid = fmap (S.fromList . map unValue) $
    select $
        from $ \g -> do
        where_ (g^.GrokkedUserId ==. val uid)
        return (g^.GrokkedResId)

getFavoriteResourcesIn :: [ResourceId] -> UserId -> YesodDB App (Set ResourceId)
getGrokkedResourcesIn  :: [ResourceId] -> UserId -> YesodDB App (Set ResourceId)

getFavoriteResourcesIn resourceIds userId = fmap (S.fromList . map unValue) $
    select $
        from $ \f -> do
        where_ (f^.FavoriteUserId ==. val userId &&.
                f^.FavoriteResId `in_` valList resourceIds)
        return (f^.FavoriteResId)

getGrokkedResourcesIn resourceIds userId = fmap (S.fromList . map unValue) $
    select $
        from $ \g -> do
        where_ (g^.GrokkedUserId ==. val userId &&.
                g^.GrokkedResId `in_` valList resourceIds)
        return (g^.GrokkedResId)

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


getPostedResources :: UserId -> YesodDB App [Entity Resource]
getPostedResources uid =
    select $
        from $ \(u `InnerJoin` r) -> do
        on (u^.UserId ==. r^.ResourceUserId)
        where_ (u^.UserId ==. val uid)
        return r

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
