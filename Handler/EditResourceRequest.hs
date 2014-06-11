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
        update $ \r -> do
            set r [ ResourceTitle =. val title ]
            where_ (r^.ResourceId ==. val resId)
        P.delete eid


postEditTitleDeclineR :: EditTitleId -> Handler Html
postEditTitleDeclineR eid = editRes editTitleResId (\_ -> P.delete eid) eid

postEditAuthorAcceptR :: EditAuthorId -> Handler Html
postEditAuthorAcceptR eid = editRes editAuthorResId sqlCode eid
  where
    sqlCode (EditAuthor resId author) = do
        update $ \r -> do
            set r [ ResourceAuthor =. val author ]
            where_ (r^.ResourceId ==. val resId)
        deleteEditAuthors author

postEditAuthorDeclineR :: EditAuthorId -> Handler Html
postEditAuthorDeclineR = editRes editAuthorResId (deleteEditAuthors . editAuthorAuthor)

-- Since a res+author edit is not unique, just delete all edits
-- with the same author.
deleteEditAuthors :: Maybe Text -> YesodDB App ()
deleteEditAuthors author =
    delete $
        from $ \e -> do
        where_ (e^.EditAuthorAuthor ==. val author)

postEditTypeAcceptR :: EditTypeId -> Handler Html
postEditTypeAcceptR eid = editRes editTypeResId sqlCode eid
  where
    sqlCode (EditType resId typ) = do
        update $ \r -> do
            set r [ ResourceType =. val typ ]
            where_ (r^.ResourceId ==. val resId)
        P.delete eid

postEditTypeDeclineR :: EditTypeId -> Handler Html
postEditTypeDeclineR eid = editRes editTypeResId (\_ -> P.delete eid) eid

postEditAddTagAcceptR :: EditAddTagId -> Handler Html
postEditAddTagAcceptR eid = editRes editAddTagResId sqlCode eid
  where
    sqlCode (EditAddTag resId text) = do
        tagId <- insertBy' (Tag text)
        void . insertUnique $ ResourceTag resId tagId
        P.delete eid

postEditAddTagDeclineR :: EditAddTagId -> Handler Html
postEditAddTagDeclineR eid = editRes editAddTagResId (\_ -> P.delete eid) eid

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
                    P.delete tagId
        P.delete eid

postEditRemoveTagDeclineR :: EditRemoveTagId -> Handler Html
postEditRemoveTagDeclineR eid = editRes editRemoveTagResId (\_ -> P.delete eid) eid

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
