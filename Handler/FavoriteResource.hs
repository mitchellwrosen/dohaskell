module Handler.FavoriteResource where

import Import

postFavoriteResourceR :: Handler Html
postFavoriteResourceR = do
    (resId, uid) <- helper
    void . runDB . insertUnique $ Favorite uid resId
    return "ok"

postUnfavoriteResourceR :: Handler Html
postUnfavoriteResourceR = do
    (resId, uid) <- helper
    runDB $ deleteBy (UniqueFavorite uid resId)
    return "ok"

-- TODO: better name
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
