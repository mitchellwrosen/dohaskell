{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import           Model.Resource
import           Model.User           (thisUserHasAuthorityOverDB)
import           View.Resource

import qualified Data.Set             as S
import           Data.Text            (intercalate)
import           Database.Persist.Sql

getResourceR :: ResourceId -> Handler Html
getResourceR res_id = do
    (res, tags, authors) <- runDB $ (,,)
        <$> get404 res_id
        <*> fetchResourceTagsDB res_id
        <*> (map authorName <$> fetchResourceAuthorsDB res_id)

    let info_widget = resourceInfoWidget (Entity res_id res)
        edit_widget =
          editResourceFormWidget
            res_id
            (Just $ resourceTitle res)
            (Just $ authors)
            (Just $ resourcePublished res)
            (Just $ resourceType res)
            (Just $ tags)

    defaultLayout $ do
        setTitle . toHtml $ "dohaskell | " <> resourceTitle res
        $(widgetFile "resource")

getEditResourceR :: ResourceId -> Handler Html
getEditResourceR res_id = do
    (res, tags, authors) <- runDB $ (,,)
        <$> get404 res_id
        <*> fetchResourceTagsDB res_id
        <*> (map authorName <$> fetchResourceAuthorsDB res_id)
    defaultLayout $
        editResourceFormWidget
          res_id
          (Just $ resourceTitle res)
          (Just $ authors)
          (Just $ resourcePublished res)
          (Just $ resourceType res)
          (Just $ tags)

postEditResourceR :: ResourceId -> Handler Html
postEditResourceR res_id = do
    res <- runDB $ get404 res_id
    ((result, _), _) <- runFormPost (editResourceForm Nothing Nothing Nothing Nothing Nothing)
    case result of
        FormSuccess (new_title, new_authors, new_published, new_type, new_tags) -> do
            ok <- thisUserHasAuthorityOverDB (resourceUserId res)
            if ok
                then do
                    runDB $ updateResourceDB
                              res_id
                              new_title
                              (map Author $ new_authors)
                              new_published
                              new_type
                              (map Tag $ new_tags)
                    setMessage "Resource updated."
                    redirect $ ResourceR res_id
                -- An authenticated, unprivileged user is the same as an
                -- unauthenticated user - their edits result in pending
                -- edits.
                else doPendingEdit res
          where
            doPendingEdit :: Resource -> Handler Html
            doPendingEdit Resource{..} = do
                pendingEditField resourceTitle     new_title     EditTitle
                pendingEditField resourcePublished new_published EditPublished
                pendingEditField resourceType      new_type      EditType

                (old_authors, old_tags) <- runDB $ (,)
                    <$> (map authorName <$> fetchResourceAuthorsDB res_id)
                    <*> fetchResourceTagsDB res_id

                -- Authors are a little different than tags, because order matters. So,
                -- we don't duplicate the fine-grained tag edits (individual add/remove),
                -- but rather, if *anything* about the authors changed, just make an edit
                -- containing all of them.
                when (old_authors /= new_authors) $
                    void $ runDB $ insertUnique (EditAuthors res_id new_authors)

                let new_tags_set = S.fromList new_tags
                    old_tags_set = S.fromList old_tags
                insertEditTag new_tags_set old_tags_set EditAddTag    -- find any NEW not in OLD: pending ADD.
                insertEditTag old_tags_set new_tags_set EditRemoveTag -- find any OLD not in NEW: pending REMOVE.

                setMessage "Your edit has been submitted for approval. Thanks!"
                redirect $ ResourceR res_id
              where
                pendingEditField :: (Eq a, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                    => a                        -- Old field value
                    -> a                        -- New field value
                    -> (ResourceId -> a -> val) -- PersistEntity constructor
                    -> Handler ()
                pendingEditField old_value new_value entityConstructor =
                    when (old_value /= new_value) $
                        void . runDB . insertUnique $ entityConstructor res_id new_value

                -- If we find any needles NOT in the haystack, insert the needle into the database
                -- with the supplied constructor.
                insertEditTag :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                               => Set Text
                               -> Set Text
                               -> (ResourceId -> Text -> val)
                               -> Handler ()
                insertEditTag needles haystack entityConstructor =
                    mapM_ (\needle ->
                        unless (S.member needle haystack) $
                            (void . runDB . insertUnique $ entityConstructor res_id needle)) needles
        FormFailure errs -> do
            setMessage . toHtml $ "Form error: " <> intercalate ", " errs
            redirect $ ResourceR res_id
        FormMissing -> redirect $ ResourceR res_id

postFavoriteResourceR,   postGrokkedResourceR   :: Handler Html
postUnfavoriteResourceR, postUngrokkedResourceR :: Handler Html
postFavoriteResourceR   = helper favoriteResourceDB
postGrokkedResourceR    = helper grokResourceDB
postUnfavoriteResourceR = helper unfavoriteResourceDB
postUngrokkedResourceR  = helper ungrokResourceDB

helper :: (UserId -> ResourceId -> YesodDB App ()) -> Handler Html
helper action = do
    user_id <- requireAuthId
    res_id  <- Key . PersistInt64 <$> runInputPost (ireq intField "res_id")
    runDB $ action user_id res_id
    return "ok"
