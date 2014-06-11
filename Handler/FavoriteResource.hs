module Handler.FavoriteResource where

import Import

import Database.Persist.Sql

postFavoriteResourceR :: Handler Html
postFavoriteResourceR = attribute Favorite

postUnfavoriteResourceR :: Handler Html
postUnfavoriteResourceR = unattribute UniqueFavorite

postGrokkedResourceR :: Handler Html
postGrokkedResourceR = attribute Grokked

postUngrokkedResourceR :: Handler Html
postUngrokkedResourceR = unattribute UniqueGrokked

-- TODO: better name for all of these
attribute :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
          => (UserId -> ResourceId -> val)
          -> Handler Html
attribute constructor = do
    (resId, uid) <- helper
    void . runDB . insertUnique $ constructor uid resId
    return "ok"

unattribute :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => (UserId -> ResourceId -> Unique val)
            -> Handler Html
unattribute constructor = do
    (resId, uid) <- helper
    runDB $ deleteBy (constructor uid resId)
    return "ok"

helper :: Handler (ResourceId, UserId)
helper = do
    resId <- Key . PersistInt64 <$> runInputPost (ireq intField "resId")
    res   <- runDB $ get404 resId
    maybeAuthId >>= \case
        Nothing -> permissionDenied "Please log in."
        Just uid -> do
            when (uid /= resourceUserId res) $
                permissionDenied "You don't have permission to view this page."
            return (resId, uid)
