module Handler.Submit where

import Import

import Database.Persist.Class.Extra (insertBy')
import View.Resource                (resourceForm)

getSubmitR :: Handler Html
getSubmitR = do
    uid <- requireAuthId
    (widget, enctype) <- generateFormPost (resourceForm uid)
    defaultLayout $ do
        setTitle "dohaskell | submit"
        $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
    ((result, _), _) <- runFormPost . resourceForm =<< requireAuthId
    case result of
        FormSuccess (title, url, authors, year, typ, tags, user_id, posted_at) -> do
            let res = Resource title url year typ user_id posted_at
            runDB (insertBy res) >>= \case
                Left (Entity res_id _) -> do
                    setDuplicateUrlMessage (resourceUrl res) res_id
                    redirect SubmitR
                Right res_id -> do
                    runDB $ do
                        mapM_ (insertResourceTagAndTag res_id) tags
                        mapM_ (insertResAuthorAndAuthor res_id) (zip [0..] authors)
                    setMessage "Thanks for your submission!"
                    redirect HomeR
        _ -> do
            setMessage "Invalid resource submission! Please try again."
            redirect SubmitR
  where
    insertResourceTagAndTag :: ResourceId -> Text -> YesodDB App ()
    insertResourceTagAndTag res_id tag = void $ insertBy' (Tag tag) >>= insertUnique . ResourceTag res_id

    insertResAuthorAndAuthor :: ResourceId -> (Int, Text) -> YesodDB App ()
    insertResAuthorAndAuthor res_id (n, name) = do
        auth_id <- insertBy' (Author name)
        void . insertUnique $ ResAuthor res_id auth_id n

    setDuplicateUrlMessage :: Text -> ResourceId -> Handler ()
    setDuplicateUrlMessage resUrl resId = do
        giveUrlRenderer [hamlet|
                URL "#{resUrl}" already exists.
                <a href=@{ResourceR resId}>Take me there.
            |] >>= setMessage
