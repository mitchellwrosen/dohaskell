module Handler.Edit where

import Import

import Data.Text                 (intercalate)

import Model.PendingResourceEdit (insertPendingResourceEdit)
import Model.Resource            (getResourceTags, updateResource)
import Model.User                (unsafeGetUserById, userHasAuthorityOver)
import View.Resource             (resourceTagsForm, resourceTypeField, resourceWidget')

getEditR :: ResourceId -> Handler Html
getEditR resId = do
    res  <- runDB $ get404 resId
    tags <- runDB $ getResourceTags resId
    (widget, enctype) <- generateFormPost (editForm (Just $ resourceTitle res)
                                                    (Just $ resourceType res)
                                                    (Just $ map tagText tags))
    defaultLayout $ do
        setTitle "Edit Resource"
        $(widgetFile "edit")

postEditR :: ResourceId -> Handler Html
postEditR resId = do
    res <- runDB $ get404 resId
    ((result, _), _) <- runFormPost (editForm Nothing Nothing Nothing)
    case result of
        FormSuccess (newTitle, newType, newTags) -> do
            maybeAuthId >>= \case
                -- It's okay to edit resources anonymously.
                Nothing -> doPendingEdit
                Just editorUid -> do
                    ok <- userHasAuthorityOver editorUid (resourceUserId res)
                    if ok
                        then do
                            runDB $ updateResource resId newTitle newType newTags 
                            setMessage "Resource updated."
                            redirect $ ResourceR resId
                        -- An authenticated, unprivileged user is the same as an
                        -- unauthenticated user - their edits result in pending
                        -- edits.
                        else doPendingEdit
          where
            doPendingEdit :: Handler Html
            doPendingEdit = do
                runDB $ insertPendingResourceEdit resId newTitle newType newTags
                setMessage "Your edit has been submitted for approval. Thanks!"
                redirect $ ResourceR resId
        FormFailure errs -> do
            setMessage . toHtml $ "Form error: " <> intercalate ", " errs
            redirect $ EditR resId
        FormMissing -> redirect $ EditR resId

editForm :: Maybe Text          -- default title
         -> Maybe ResourceType  -- default type
         -> Maybe [Text]        -- default tags
         -> Form (Text, ResourceType, [Tag])
editForm title typ tags = renderDivs $ (,,)
    <$> areq textField         "Title" title
    <*> areq resourceTypeField "Type"  typ
    <*> resourceTagsForm tags
