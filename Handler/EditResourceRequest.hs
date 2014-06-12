module Handler.EditResourceRequest where

import Import

import           Database.Esqueleto
import qualified Database.Persist   as P

import Database.Persist.Class.Extra (insertBy')
import Handler.Utils                (denyPermissionIfDoesntHaveAuthorityOver)

postEditTitleAcceptR :: EditTitleId -> Handler Html
postEditTitleAcceptR eid = editRes editTitleResId sqlCode eid
  where
    sqlCode (EditTitle resId title) = do
        updateResField resId ResourceTitle title
        deleteKey eid

postEditTitleDeclineR :: EditTitleId -> Handler Html
postEditTitleDeclineR eid = editRes editTitleResId (\_ -> deleteKey eid) eid

postEditAuthorAcceptR :: EditAuthorId -> Handler Html
postEditAuthorAcceptR eid = editRes editAuthorResId sqlCode eid
  where
    sqlCode (EditAuthor resId author) = do
        updateResField resId ResourceAuthor author
        deleteNullable EditAuthorAuthor author

postEditAuthorDeclineR :: EditAuthorId -> Handler Html
postEditAuthorDeclineR = editRes editAuthorResId (deleteNullable EditAuthorAuthor . editAuthorAuthor)

postEditPublishedAcceptR :: EditPublishedId -> Handler Html
postEditPublishedAcceptR eid = editRes editPublishedResId sqlCode eid
  where
    sqlCode (EditPublished resId published) = do
        updateResField resId ResourcePublished published
        deleteNullable EditPublishedPublished published

postEditPublishedDeclineR :: EditPublishedId -> Handler Html
postEditPublishedDeclineR = editRes editPublishedResId (deleteNullable EditPublishedPublished . editPublishedPublished)

postEditTypeAcceptR :: EditTypeId -> Handler Html
postEditTypeAcceptR eid = editRes editTypeResId sqlCode eid
  where
    sqlCode (EditType resId typ) = do
        updateResField resId ResourceType typ
        deleteKey eid

postEditTypeDeclineR :: EditTypeId -> Handler Html
postEditTypeDeclineR eid = editRes editTypeResId (\_ -> deleteKey eid) eid

postEditAddTagAcceptR :: EditAddTagId -> Handler Html
postEditAddTagAcceptR eid = editRes editAddTagResId sqlCode eid
  where
    sqlCode (EditAddTag resId text) = do
        tagId <- insertBy' (Tag text)
        void . insertUnique $ ResourceTag resId tagId
        deleteKey eid

postEditAddTagDeclineR :: EditAddTagId -> Handler Html
postEditAddTagDeclineR eid = editRes editAddTagResId (\_ -> deleteKey eid) eid

postEditRemoveTagAcceptR :: EditRemoveTagId -> Handler Html
postEditRemoveTagAcceptR eid = editRes editRemoveTagResId sqlCode eid
  where
    sqlCode (EditRemoveTag resId text) = do
        getBy (UniqueTagText text) >>= \case
            Nothing -> return ()
            Just (Entity tagId _) -> do
                -- Delete the resource-tag relationship, and possibly also
                -- delete the tag (if no other resources share it)
                deleteBy $ UniqueResourceTag resId tagId
                n <- P.count [ResourceTagTagId P.==. tagId]
                when (n == 0) $
                    deleteKey tagId
        deleteKey eid

postEditRemoveTagDeclineR :: EditRemoveTagId -> Handler Html
postEditRemoveTagDeclineR eid = editRes editRemoveTagResId (\_ -> deleteKey eid) eid

-- Utility method shared by resource edit functions. Performs boiler
-- plate code such as 404 on invalid edit id, resource id, and deny permission
-- if necessary. Runs arbitrary SQL which is responsible for deleting the edit.

editRes :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
           => (val -> ResourceId)     -- ^ ResourceId accessor
           -> (val -> YesodDB App ()) -- ^ Arbitrary SQL code to run, given val.
           -> Key val
           -> Handler b
editRes getResourceFunc sqlCode eid = do
    (edit, res) <- runDB $ do
      edit <- get404 eid
      res  <- get404 $ getResourceFunc edit
      return (edit, res)

    -- Admins and the user himself may accept an edit.
    denyPermissionIfDoesntHaveAuthorityOver $ resourceUserId res

    runDB $ sqlCode edit
    redirectUltDest HomeR

updateResField resId field value = do
    update $ \r -> do
        set r [ field =. val value ]
        where_ (r^.ResourceId ==. val resId)

-- Since a field is not unique (it's nullable), delete all fields with the given value.
deleteNullable field value =
    delete $
        from $ \e -> do
        where_ (e^.field ==. val value)
