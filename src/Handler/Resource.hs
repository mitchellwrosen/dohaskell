{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import           Model.List
import           Model.Resource
import           Model.User           (thisUserHasAuthorityOverDB)
import           View.Browse
import           View.Resource

import qualified Data.Set             as S

getResourceR :: ResourceId -> Handler Html
getResourceR res_id = do
    (res@Resource{..}, authors, tags, colls) <- runDB $ (,,,)
        <$> get404 res_id
        <*> (map authorName     <$> fetchResourceAuthorsDB res_id)
        <*> (map tagName        <$> fetchResourceTagsDB res_id)
        <*> (map collectionName <$> fetchResourceCollectionsDB res_id)

    let info_widget = resourceInfoWidget (Entity res_id res)
        edit_widget =
          editResourceFormWidget
            res_id
            (Just resourceTitle)
            (Just authors)
            (Just resourcePublished)
            (Just resourceType)
            (Just tags)
            (Just colls)

    defaultLayout $ do
        setTitle . toHtml $ "dohaskell | " <> resourceTitle
        $(widgetFile "resource")

getEditResourceR :: ResourceId -> Handler Html
getEditResourceR res_id = do
    (Resource{..}, authors, tags, colls) <- runDB $ (,,,)
        <$> get404 res_id
        <*> (map authorName     <$> fetchResourceAuthorsDB res_id)
        <*> (map tagName        <$> fetchResourceTagsDB res_id)
        <*> (map collectionName <$> fetchResourceCollectionsDB res_id)

    defaultLayout $
        editResourceFormWidget
          res_id
          (Just resourceTitle)
          (Just authors)
          (Just resourcePublished)
          (Just resourceType)
          (Just tags)
          (Just colls)

postEditResourceR :: ResourceId -> Handler Html
postEditResourceR res_id = do
    res <- runDB (get404 res_id)
    ((result, _), _) <- runFormPost (editResourceForm Nothing Nothing Nothing Nothing Nothing Nothing)
    case result of
        FormSuccess (new_title, new_authors, new_published, new_type, new_tags, new_colls) -> do
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
                              (map Collection $ new_colls)
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

                (old_authors, old_tags, old_colls) <- runDB $ (,,)
                    <$> (map authorName     <$> fetchResourceAuthorsDB res_id)
                    <*> (map tagName        <$> fetchResourceTagsDB res_id)
                    <*> (map collectionName <$> fetchResourceCollectionsDB res_id)

                -- Authors are a little different than tags/collections, because
                -- order matters. So, -- we don't duplicate the fine-grained tag
                -- edits (individual add/remove), but rather, if *anything*
                -- about the authors changed, just make an edit containing all
                -- of them.
                when (old_authors /= new_authors) $
                    void $ runDB (insertUnique (EditAuthors res_id new_authors))

                pendingEditRelation (S.fromList new_tags)  (S.fromList old_tags)  EditAddTag        EditRemoveTag
                pendingEditRelation (S.fromList new_colls) (S.fromList old_colls) EditAddCollection EditRemoveCollection

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

                pendingEditRelation :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend,
                                        PersistEntity b, PersistEntityBackend b ~ SqlBackend,
                                        Ord field)
                                    => Set field
                                    -> Set field
                                    -> (ResourceId -> field -> a)
                                    -> (ResourceId -> field -> b)
                                    -> Handler ()
                pendingEditRelation new_fields old_fields edit_add edit_remove = do
                    pendingEditRelation' new_fields old_fields edit_add    -- find any NEW not in OLD: pending ADD.
                    pendingEditRelation' old_fields new_fields edit_remove -- find any OLD not in NEW: pending REMOVE.

                -- If we find any needles NOT in the haystack, insert the needle into the database
                -- with the supplied constructor.
                pendingEditRelation' :: (Ord field, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                                     => Set field
                                     -> Set field
                                     -> (ResourceId -> field -> val)
                                     -> Handler ()
                pendingEditRelation' needles haystack edit_constructor =
                    forM_ needles $ \needle ->
                        unless (S.member needle haystack) $
                            void . runDB . insertUnique $ edit_constructor res_id needle

        FormFailure errs -> do
            setMessage . toHtml $ "Form error: " <> intercalate ", " errs
            redirect $ ResourceR res_id
        FormMissing -> redirect $ ResourceR res_id

getResourceListR :: Text -> Handler Html
getResourceListR list_name =
    requireAuthId
      >>= runDB . flip fetchListResourcesDB list_name
        >>= defaultLayout . resourceListWidget

postResourceListAddR, postResourceListDelR :: Text -> ResourceId -> Handler Html
postResourceListAddR = postResourceListAddDel addListItemDB
postResourceListDelR = postResourceListAddDel deleteListItemDB

postResourceListAddDel :: (UserId -> Text -> ResourceId -> YesodDB App ()) -> Text -> ResourceId -> Handler Html
postResourceListAddDel action list_name res_id = do
    user_id <- requireAuthId
    runDB (action user_id list_name res_id)
    return "ok"
