module Handler.Utils
    ( SortBy(..)
    , addGetParam
    , getCurrentRouteWithGetParams
    , denyPermissionIfDifferentUser
    , denyPermissionIfDoesntHaveAuthorityOver
    , denyPermissionIfNotAdmin
    ) where

import Import

import           Model.Browse
import           Model.User   (isAdministratorDB, userHasAuthorityOverDB)

import           Data.Maybe   (fromJust)
import qualified Data.Map     as M

denyPermissionIfDifferentUser :: UserId -> Handler ()
denyPermissionIfDifferentUser requestedUser = maybeAuthId >>= \case
    Nothing -> deny
    Just thisUser ->
        runDB (get requestedUser) >>= \case
            Nothing -> notFound
            Just _  -> when (requestedUser /= thisUser)
                           deny

denyPermissionIfDoesntHaveAuthorityOver :: UserId -> Handler ()
denyPermissionIfDoesntHaveAuthorityOver nerd = maybeAuthId >>= \case
    Nothing -> deny
    Just bully ->
        runDB (get nerd) >>= \case
            Nothing -> notFound
            Just _  -> do
                ok <- runDB $ userHasAuthorityOverDB bully nerd
                when (not ok)
                    deny

denyPermissionIfNotAdmin :: Handler ()
denyPermissionIfNotAdmin = maybeAuthId >>= \case
    Nothing  -> deny
    Just uid -> runDB (isAdministratorDB uid) >>= \b -> unless b deny

deny :: Handler ()
deny = permissionDenied "You don't have permission to view this page."

-- | Get the current route with the current GET params.
-- Unsafe if getCurrentRoute would return Nothing.
getCurrentRouteWithGetParams :: Handler (Route App, [(Text, Text)])
getCurrentRouteWithGetParams = (,)
    <$> (fromJust <$> getCurrentRoute)
    <*> (reqGetParams <$> getRequest)

-- | Add a new GET param to a list of GET params.
addGetParam :: (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
addGetParam (k,v) = M.toList . M.insert k v . M.fromList
