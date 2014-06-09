module Handler.Utils 
    ( denyPermissionIfDifferentUser
    , denyPermissionIfDoesntHaveAuthorityOver
    , denyPermissionIfNotAdmin
    , editAccept
    , editDecline
    , lookupIntPostParam
    ) where

import Import

import           Data.Attoparsec.Text (decimal, parseOnly)
import qualified Data.Text            as T
import qualified Database.Persist     as P

import Model.User (getUserById, isAdministrator, userHasAuthorityOver)

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

{-denyPermissionIfDoesntOwnResource :: ResourceId -> Handler ()-}
{-denyPermissionIfDoesntOwnResource resId = maybeAuthId >>= \case-}
    {-Nothing -> deny-}
    {-Just uid -> do-}
        {-res <- runDB $ get404 resId-}
        {-ok <- userHasAuthorityOver uid (resourceUserId res)-}
        {-when (not ok) deny-}

denyPermissionIfNotAdmin :: Handler ()
denyPermissionIfNotAdmin = maybeAuthId >>= \case
    Nothing -> deny
    Just uid -> isAdministrator uid >>= \b -> if b then return () else deny

deny :: Handler ()
deny = permissionDenied "You don't have permission to view this page."

-- Utility method shared by all 'accept resource edit' functions. Performs boiler
-- plate code such as 404 on invalid edit id, resource id, and deny permission
-- if necessary. Deletes the edit from the database and redirects to the edit hub.
editAccept :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
           => (val -> ResourceId)     -- ^ ResourceId accessor
           -> (val -> YesodDB App ()) -- ^ Arbitrary SQL code to run, given val.
           -> Key val
           -> Handler b
editAccept getResourceFunc sqlCode eid = do
    edit <- runDB $ get404 eid
    res <- runDB . get404 $ getResourceFunc edit
    let uid = resourceUserId res

    -- Admins and the user himself may accept an edit.
    denyPermissionIfDoesntHaveAuthorityOver uid

    runDB $ do
        sqlCode edit
        P.delete eid
    redirectUltDest HomeR

-- Utility method shared by all 'decline resource edit' functions. Performs boiler
-- plate code such as 404 on invalid edit id, resource id, and deny permission
-- if necessary. Deletes the edit from the database and redirects to the edit hub.
editDecline :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => (val -> ResourceId) -- ^ ResourceId accessor
            -> Key val
            -> Handler a
editDecline getResourceFunc eid = do
    res <- runDB $ get404 eid >>= get404 . getResourceFunc
    let uid = resourceUserId res

    -- Admins and the user himself may decline an edit.
    denyPermissionIfDoesntHaveAuthorityOver uid

    runDB $ P.delete eid
    redirectUltDest HomeR

-- TODO: put in Yesod.Core.Handler.Extra?
lookupIntPostParam :: (MonadHandler m, Integral a) => Text -> m a
lookupIntPostParam key = lookupPostParam key >>= \case
    Nothing    -> invalidArgs $ ["Missing POST param: " <> key]
    Just value -> case parseOnly decimal value of
        Left err -> invalidArgs $ ["Bad POST param: " <> key <> " (" <> T.pack err <> ")"]
        Right n  -> return n
