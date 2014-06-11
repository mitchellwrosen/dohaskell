module Model.User
    ( getPostedResources
    , getUserById
    , isAdministrator
    , thisUserHasAuthorityOver
    , unsafeGetUserById
    , updateUserDisplayName
    , userHasAuthorityOver
    ) where

import Import

import Database.Esqueleto

import           Control.Concurrent (modifyMVar_, readMVar)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)

getPostedResources :: UserId -> YesodDB App [Entity Resource]
getPostedResources uid =
    select $
        from $ \(u `InnerJoin` r) -> do
        on (u^.UserId ==. r^.ResourceUserId)
        where_ (u^.UserId ==. val uid)
        return r

-- Get a User given a UserId; look in memory first. Grab from the database and
-- put into memory if necessary.
getUserById :: UserId -> Handler (Maybe User)
getUserById uid = do
    usersMap <- appUsersMap <$> getYesod
    M.lookup uid <$> liftIO (readMVar usersMap) >>= \case
        Nothing ->
            runDB (get uid) >>= \case
                Nothing   -> return Nothing
                Just user -> do
                    liftIO $ modifyMVar_ usersMap $ return . M.insert uid user
                    return (Just user)
        user@(Just _) -> return user

isAdministrator :: UserId -> Handler Bool
isAdministrator = fmap (maybe False userIsAdministrator) . getUserById

-- Partial function. Should only be used when the UserId is (for example)
-- a foreign key, and thus if this function bottoms, there's some database
-- inconsistency.
unsafeGetUserById :: UserId -> Handler User
unsafeGetUserById = fmap fromJust . getUserById

updateUserDisplayName :: UserId -> Text -> Handler ()
updateUserDisplayName uid displayName = do
    -- Keep in-memory cache up to date by deleting old entry.
    modifyUsersMap (M.delete uid)

    runDB $ update $ \user -> do
        set user [UserDisplayName =. val displayName]
        where_ (user^.UserId ==. val uid)

-- 'bully' has authority over 'nerd' if 'bully' is an administrator,
-- or of 'bully' and 'nerd' are the same user.
--
-- Assumes that 'bully' is an actual user id, not from a URL.
userHasAuthorityOver :: UserId -> UserId -> Handler Bool
userHasAuthorityOver bully nerd = do
    isAdmin <- userIsAdministrator <$> unsafeGetUserById bully
    return $
        if isAdmin
            then True
            else (bully == nerd)

-- Like userHasAuthorityOver, but uses the current user ('this' user) as the
-- first argument.
thisUserHasAuthorityOver :: UserId -> Handler Bool
thisUserHasAuthorityOver nerd = maybeAuthId >>= \case
    Nothing    -> return False
    Just bully -> userHasAuthorityOver bully nerd
