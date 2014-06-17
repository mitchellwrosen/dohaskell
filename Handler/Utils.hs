module Handler.Utils
    ( alphabeticIgnoreCase
    , denyPermissionIfDifferentUser
    , denyPermissionIfDoesntHaveAuthorityOver
    , denyPermissionIfNotAdmin
    ) where

import Import

import qualified Data.Text as T

import Model.User (getUserById, isAdministrator, userHasAuthorityOver)

-- TODO: Find a better module for this function.
-- Requires that
alphabeticIgnoreCase :: (val -> Text) -> Entity val -> Entity val -> Ordering
alphabeticIgnoreCase textFunc (Entity _ val1) (Entity _ val2) =
    T.toLower (textFunc val1) `compare` T.toLower (textFunc val2)

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

denyPermissionIfNotAdmin :: Handler ()
denyPermissionIfNotAdmin = maybeAuthId >>= \case
    Nothing -> deny
    Just uid -> isAdministrator uid >>= \b -> if b then return () else deny

deny :: Handler ()
deny = permissionDenied "You don't have permission to view this page."
