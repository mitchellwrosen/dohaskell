module Handler.EditResourceRequest where

import Import

import           Database.Persist.Class.Extra (insertBy')
import           Handler.Utils                (denyPermissionIfDoesntHaveAuthorityOver)
import           Model.Resource               (updateResourceAuthorsDB)

import           Database.Esqueleto
import qualified Database.Persist   as P


-- Adding a title/type is the same - they're both required fields.

postEditTitleAcceptR :: EditTitleId -> Handler Html
postEditTitleAcceptR eid = editRes editTitleResId sqlCode eid
  where
    sqlCode (EditTitle resId title) = updateResField resId ResourceTitle title

postEditTypeAcceptR  :: EditTypeId  -> Handler Html
postEditTypeAcceptR eid = editRes editTypeResId sqlCode eid
  where
    sqlCode (EditType resId typ) = updateResField resId ResourceType typ

postEditAuthorsAcceptR :: EditAuthorsId -> Handler Html
postEditAuthorsAcceptR eid = editRes editAuthorsResId sqlCode eid
  where
    sqlCode (EditAuthors res_id author_names) = updateResourceAuthorsDB res_id (map Author author_names)

postEditAddTagAcceptR :: EditAddTagId -> Handler Html
postEditAddTagAcceptR eid = editRes editAddTagResId sqlCode eid
  where
    sqlCode (EditAddTag resId text) = insertBy' (Tag text) >>= void . insertUnique . ResourceTag resId

postEditRemoveTagAcceptR :: EditRemoveTagId -> Handler Html
postEditRemoveTagAcceptR eid = editRes editRemoveTagResId sqlCode eid
  where
    sqlCode (EditRemoveTag resId text) = do
        getBy (UniqueTag text) >>= \case
            Nothing -> return ()
            Just (Entity tagId _) ->
                deleteOneToMany
                    (UniqueResourceTag resId tagId)
                    ResourceTagTagId
                    tagId

-- TODO: Share code with add/remove tag, as above.
postEditAddCollectionAcceptR :: EditAddCollectionId -> Handler Html
postEditAddCollectionAcceptR eid = editRes editAddCollectionResId sqlCode eid
  where
    sqlCode (EditAddCollection resId text) = insertBy' (Collection text) >>= void . insertUnique . ResCollection resId

postEditRemoveCollectionAcceptR :: EditRemoveCollectionId -> Handler Html
postEditRemoveCollectionAcceptR eid = editRes editRemoveCollectionResId sqlCode eid
  where
    sqlCode (EditRemoveCollection resId text) = do
        getBy (UniqueCollection text) >>= \case
            Nothing -> return ()
            Just (Entity colId _) ->
                deleteOneToMany
                    (UniqueResCollection resId colId)
                    ResCollectionColId
                    colId

-- Delete one of many entities, and delete the entity it references by its
-- id if it happened to be the only one in the one-to-many relation.
deleteOneToMany uniqueEntity idField idVal = do
    deleteBy uniqueEntity
    n <- P.count [idField P.==. idVal]
    when (n == 0) $
        deleteKey idVal

postEditPublishedAcceptR :: EditPublishedId -> Handler Html
postEditPublishedAcceptR eid = editRes editPublishedResId sqlCode eid
  where
    sqlCode (EditPublished resId published) = updateResField resId ResourcePublished published

postEditAddCollectionDeclineR    :: EditAddCollectionId    -> Handler Html
postEditAddTagDeclineR           :: EditAddTagId           -> Handler Html
postEditAuthorsDeclineR          :: EditAuthorsId          -> Handler Html
postEditPublishedDeclineR        :: EditPublishedId        -> Handler Html
postEditRemoveTagDeclineR        :: EditRemoveTagId        -> Handler Html
postEditRemoveCollectionDeclineR :: EditRemoveCollectionId -> Handler Html
postEditTitleDeclineR            :: EditTitleId            -> Handler Html
postEditTypeDeclineR             :: EditTypeId             -> Handler Html

postEditAddCollectionDeclineR    = declineEditRes editAddCollectionResId
postEditAddTagDeclineR           = declineEditRes editAddTagResId
postEditAuthorsDeclineR          = declineEditRes editAuthorsResId
postEditPublishedDeclineR        = declineEditRes editPublishedResId
postEditRemoveCollectionDeclineR = declineEditRes editRemoveCollectionResId
postEditRemoveTagDeclineR        = declineEditRes editRemoveTagResId
postEditTitleDeclineR            = declineEditRes editTitleResId
postEditTypeDeclineR             = declineEditRes editTypeResId

declineEditRes :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
               => (val -> ResourceId)
               -> Key val
               -> Handler a
declineEditRes = flip editRes (\_ -> return ())

-- Utility method shared by resource edit functions. Performs boiler
-- plate code such as 404 on invalid edit id, resource id, and deny permission
-- if necessary. Runs arbitrary SQL, which is not responsible for deleting the
-- edit.

editRes :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
           => (val -> ResourceId)     -- ^ ResourceId accessor
           -> (val -> YesodDB App ()) -- ^ Arbitrary SQL code to run, given val.
           -> Key val
           -> Handler a
editRes getResourceFunc sqlCode eid = do
    (edit, res) <- runDB $ do
        edit <- get404 eid
        res  <- get404 $ getResourceFunc edit
        return (edit, res)

    -- Admins and the user himself may accept an edit.
    denyPermissionIfDoesntHaveAuthorityOver $ resourceUserId res

    runDB $ do
        sqlCode edit
        deleteKey eid
    redirectUltDest HomeR

updateResField resId field value = do
    update $ \r -> do
        set r [ field =. val value ]
        where_ (r^.ResourceId ==. val resId)
