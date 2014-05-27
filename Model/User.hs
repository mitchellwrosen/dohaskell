module Model.User 
    ( getUserById
    , unsafeGetUserById
    , updateUserDisplayName
    ) where

import Import

import           Control.Concurrent (modifyMVar_, readMVar)
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)

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
        set user [UserDisplayName =. val (Just displayName)]
        where_ (user^.UserId ==. val uid)

modifyUsersMap :: (Map UserId User -> Map UserId User) -> Handler ()
modifyUsersMap f = appUsersMap <$> getYesod >>= \usersMap -> liftIO (modifyMVar_ usersMap (return . f))
