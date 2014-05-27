module Handler.Submit where

import Import

import View.Navbar            (navbarWidget)
import View.Resource          (resourceForm)

getSubmitR :: Handler Html
getSubmitR = do
    uid <- requireAuthId
    (widget, enctype) <- generateFormPost (resourceForm uid)
    defaultLayout $ do
        setTitle "Submit a resource"
        $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
    ((result, _), _) <- runFormPost . resourceForm =<< requireAuthId
    case result of
        FormSuccess (res,tags) -> do
            runDB (insertBy res) >>= \case
                Left (Entity resId _) -> do
                    setDuplicateUrlMessage (resourceUrl res) resId
                    redirect SubmitR
                Right resId -> do
                    -- It's okay to ignore returned ids, attempting to insert
                    -- duplicate ResourceTags is okay.
                    runDB $ mapM_ (insertResourceTagAndTag resId) tags
                    setMessage "Thanks for your submission!"
                    redirect HomeR
        _ -> do
            setMessage "Invalid resource submission! Please try again."
            redirect SubmitR
  where
    insertResourceTagAndTag :: ResourceId -> Tag -> SqlPersistT Handler (Maybe ResourceTagId)
    insertResourceTagAndTag resId tag = do
        insertBy tag >>= \case
            Left (Entity tagId _) -> insertUnique $ ResourceTag resId tagId
            Right tagId           -> insertUnique $ ResourceTag resId tagId

    setDuplicateUrlMessage :: Text -> ResourceId -> Handler ()
    setDuplicateUrlMessage resUrl resId = do
        giveUrlRenderer [hamlet|
                URL "#{resUrl}" already exists.
                <a href=@{ResourceR resId}>Take me there.
            |] >>= setMessage
