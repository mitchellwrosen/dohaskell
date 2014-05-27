module Handler.Utils where

import Import

import Model.User (getUserById)

denyPermissionIfDifferentUser :: UserId -> Handler ()
denyPermissionIfDifferentUser uid = do
    uid' <- requireAuthId
    getUserById uid >>= \case
        Nothing -> notFound
        Just _  -> if uid /= uid'
                       then permissionDenied "You don't have permission to view this page."
                       else return ()
