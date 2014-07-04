{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import           Handler.Utils        (denyPermissionIfDoesntHaveAuthorityOver)
import           Model.Resource
import           Model.User           (thisUserHasAuthorityOver)
import           View.Resource        (editResourceForm, resourceInfoWidget)

import qualified Data.Set             as S
import           Data.Text            (intercalate)
import           Database.Persist.Sql

getResourceR :: ResourceId -> Handler Html
getResourceR res_id = do
    (res,tags,authors) <- runDB $ (,,)
        <$> get404     res_id
        <*> getTags    res_id
        <*> getAuthors res_id
    (widget, enctype) <- generateFormPost $
        editResourceForm (Just $ resourceTitle res)
                         (Just $ authorName <$> authors)
                         (Just $ resourcePublished res)
                         (Just $ resourceType res)
                         (Just $ S.map tagTag tags)
    canDelete <- thisUserHasAuthorityOver (resourceUserId res)
    defaultLayout $ do
        setTitle . toHtml $ "dohaskell | " <> resourceTitle res
        $(widgetFile "resource")

postDeleteResourceR :: ResourceId -> Handler Html
postDeleteResourceR resId = do
    -- Admins and the user himself may delete a resource.
    (resourceUserId <$> runDB (get404 resId)) >>= denyPermissionIfDoesntHaveAuthorityOver

    runDB $ deleteResource resId
    setMessage "Resource deleted."
    redirect HomeR

postEditResourceR :: ResourceId -> Handler Html
postEditResourceR res_id = do
    res <- runDB $ get404 res_id
    ((result, _), _) <- runFormPost (editResourceForm Nothing Nothing Nothing Nothing Nothing)
    case result of
        FormSuccess (new_title, new_authors, new_published, new_type, new_tags) -> do
            ok <- thisUserHasAuthorityOver (resourceUserId res)
            if ok
                then do
                    runDB $ updateResource
                                res_id
                                new_title
                                (map Author $ new_authors)
                                new_published
                                new_type
                                (map Tag . S.toAscList $ new_tags)
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
                    <$> getAuthorNames res_id
                    <*> (S.map tagTag <$> getTags res_id)

                -- Authors are a little different than tags, because order matters. So,
                -- we don't duplicate the fine-grained tag edits (individual add/remove),
                -- but rather, if *anything* about the authors changed, just make an edit
                -- containing all of them.
                when (old_authors /= new_authors) $
                    void $ runDB $ insertUnique (EditAuthors res_id new_authors)

                insertEditTag new_tags old_tags EditAddTag    -- find any NEW not in OLD: pending ADD.
                insertEditTag old_tags new_tags EditRemoveTag -- find any OLD not in NEW: pending REMOVE.

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
postFavoriteResourceR   = helper favoriteResource
postGrokkedResourceR    = helper grokResource
postUnfavoriteResourceR = helper unfavoriteResource
postUngrokkedResourceR  = helper ungrokResource

helper :: (UserId -> ResourceId -> YesodDB App ()) -> Handler Html
helper action = do
    user_id <- requireAuthId
    res_id  <- Key . PersistInt64 <$> runInputPost (ireq intField "res_id")
    runDB $ action user_id res_id
    return "ok"
