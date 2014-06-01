module Handler.EditResource where

import Import

import qualified Data.Set          as S
import           Data.Text         (intercalate)

import           Model.Resource    (getResourceTags, updateResource)
import           Model.User        (userHasAuthorityOver)
import           View.EditResource (editResourceForm)

postEditResourceR :: ResourceId -> Handler Html
postEditResourceR resId = do
    res <- runDB $ get404 resId
    ((result, _), _) <- runFormPost (editResourceForm Nothing Nothing Nothing)
    case result of
        FormSuccess (newTitle, newType, newTags) -> do
            maybeAuthId >>= \case
                -- It's okay to edit resources anonymously.
                Nothing -> doPendingEdit res
                Just editorUid -> do
                    ok <- userHasAuthorityOver editorUid (resourceUserId res)
                    if ok
                        then do
                            runDB $ updateResource resId newTitle newType (map Tag . S.toAscList $ newTags)
                            setMessage "Resource updated."
                            redirect $ ResourceR resId
                        -- An authenticated, unprivileged user is the same as an
                        -- unauthenticated user - their edits result in pending
                        -- edits.
                        else doPendingEdit res
          where
            doPendingEdit :: Resource -> Handler Html
            doPendingEdit Resource{..} = do
                pendingEditField resourceTitle newTitle EditTitle
                pendingEditField resourceType newType EditType
                oldTags <- S.fromList . map tagText <$> runDB (getResourceTags resId)
                insertEditTags newTags oldTags EditAddTag    -- find any NEW not in OLD: pending ADD.
                insertEditTags oldTags newTags EditRemoveTag -- find any OLD new in NEW: pending REMOVE.
                setMessage "Your edit has been submitted for approval. Thanks!"
                redirect $ ResourceR resId
              where
                pendingEditField :: (Eq a, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                    => a                        -- Old field value
                    -> a                        -- New field value
                    -> (ResourceId -> a -> val) -- PersistEntity constructor
                    -> Handler ()
                pendingEditField oldValue newValue entityConstructor =
                    when (oldValue /= newValue) $
                        void . runDB . insertUnique $ entityConstructor resId newValue

                -- If we find any needles NOT in the haystack, insert the needle into the database
                -- with the supplied constructor.
                insertEditTags :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                               => Set Text
                               -> Set Text
                               -> (ResourceId -> Text -> val)
                               -> Handler ()
                insertEditTags needles haystack entityConstructor =
                    mapM_ (\needle ->
                        unless (S.member needle haystack) $
                            (void . runDB . insertUnique $ entityConstructor resId needle)) needles
        FormFailure errs -> do
            setMessage . toHtml $ "Form error: " <> intercalate ", " errs
            redirect $ ResourceR resId
        FormMissing -> redirect $ ResourceR resId
