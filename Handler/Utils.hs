module Handler.Utils 
    ( denyPermissionIfDifferentUser
    , denyPermissionIfDoesntHaveAuthorityOver
    , denyPermissionIfDoesntOwnResource
    ) where

import Import

import Model.User (getUserById, userHasAuthorityOver)

denyPermissionIfDifferentUser :: UserId -> Handler ()
denyPermissionIfDifferentUser requestedUser = maybeAuthId >>= \case
    Nothing -> deny
    Just thisUser ->
        getUserById requestedUser >>= \case
            Nothing -> notFound
            Just _  -> when (requestedUser /= thisUser) deny

denyPermissionIfDoesntHaveAuthorityOver :: UserId -> Handler ()
denyPermissionIfDoesntHaveAuthorityOver nerd = maybeAuthId >>= \case
    Nothing -> deny
    Just bully ->
        getUserById nerd >>= \case
            Nothing -> notFound
            Just _  -> do
                ok <- userHasAuthorityOver bully nerd
                when (not ok) deny

denyPermissionIfDoesntOwnResource :: ResourceId -> Handler ()
denyPermissionIfDoesntOwnResource resId = maybeAuthId >>= \case
    Nothing -> deny
    Just uid -> do
        res <- runDB $ get404 resId
        ok <- userHasAuthorityOver uid (resourceUserId res)
        when (not ok) deny

deny :: Handler ()
deny = permissionDenied "You don't have permission to view this page."
