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
        FormSuccess (title, url, authors, year, typ, tags, colls, user_id, posted_at) -> do
            let res = Resource title url year typ user_id posted_at
            runDB (insertBy res) >>= \case
                Left (Entity res_id _) -> do
                    setDuplicateUrlMessage (resourceUrl res) res_id
                    redirect SubmitR
                Right res_id -> do
                    runDB $ do
                        mapM_ (insertResAuthorAndAuthor res_id) (zip [0..] authors)
                        mapM_ (insertTagOrCollection Tag        ResourceTag   res_id) tags
                        mapM_ (insertTagOrCollection Collection ResCollection res_id) colls
                    setMessage "Thanks for your submission!"
                    redirect HomeR
        _ -> do
            setMessage "Invalid resource submission! Please try again."
            redirect SubmitR
  where
    insertResAuthorAndAuthor :: ResourceId -> (Int, Text) -> YesodDB App ()
    insertResAuthorAndAuthor res_id (n, name) = do
        auth_id <- insertBy' (Author name)
        void . insertUnique $ ResAuthor res_id auth_id n

    insertTagOrCollection entity relation res_id name =
        void $ insertBy' (entity name) >>= insertUnique . relation res_id


    setDuplicateUrlMessage :: Text -> ResourceId -> Handler ()
    setDuplicateUrlMessage resUrl resId = do
        giveUrlRenderer [hamlet|
                URL "#{resUrl}" already exists.
                <a href=@{ResourceR resId}>Take me there.
            |] >>= setMessage
