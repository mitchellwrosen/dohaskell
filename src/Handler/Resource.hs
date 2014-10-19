{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import qualified Data.Tree.Extra      as Tree
import           Model.Resource
import           Model.User           (thisUserHasAuthorityOverDB)
import           View.Resource

import           Data.Function        (on)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Sequence        (Seq, ViewL(..), (|>))
import qualified Data.Sequence        as Seq
import qualified Data.Set             as S
import           Data.Text            (intercalate)
import           Data.Tree            (Forest, Tree(..))
import qualified Data.Tree            as Tree
import           Database.Persist.Sql

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

getResourceCommentsR :: ResourceId -> Handler Html
getResourceCommentsR res_id = do
    (resource, comment_forest) <- runDB $ (,)
        <$> get404 res_id
        <*> (makeCommentForest <$> fetchResourceCommentsDB res_id)
    defaultLayout $ do
        setTitle . toHtml $ "dohaskell | " <> (resourceTitle resource) <> " comments"
        $(widgetFile "resource-comments")
  where
    makeCommentForest :: [Entity Comment] -> Forest (Entity Comment)
    makeCommentForest = Tree.sortForestBy orderingNewestFirst . M.elems . fst . foldl' step (mempty, mempty)
      where
        -- First map: map from comment id to the tree for which that id is the root comment.
        -- Second map: map from comment id to the "breadcrumbs" of comment ids consisting of
        --    all of its ancestors, beginning with the eldest (the root).
        --
        -- The second map allows us to traverse a tree until we find the correct spot to insert a
        -- comment, and the first map allows us to begin traversing the correct tree in the first
        -- place.
        --
        -- Because comment ids are monotonically increasing, and we are folding left to right, we
        -- know at each step exactly which comment tree a comment should end up in. That is to say,
        -- we won't come across some comment with parent id '5' and not have already inserted '5'
        -- into some tree.
        step :: (Map CommentId (Tree (Entity Comment)), Map CommentId (Seq CommentId))
             -> Entity Comment
             -> (Map CommentId (Tree (Entity Comment)), Map CommentId (Seq CommentId))
        step (roots_map, breadcrumbs_map) comment@(Entity comment_id _) =
            case commentParentId (entityVal comment) of
                Nothing ->
                    (M.insert comment_id (Tree.singleton comment)   roots_map,
                     M.insert comment_id (Seq.singleton comment_id) breadcrumbs_map)

                Just parent_id ->
                    let breadcrumbs                 = breadcrumbs_map M.! parent_id
                        root_id :< tail_breadcrumbs = Seq.viewl breadcrumbs

                    in (M.adjust (addChild (Seq.viewl tail_breadcrumbs) comment) root_id roots_map,
                        M.insert comment_id (breadcrumbs |> comment_id) breadcrumbs_map)

        addChild :: ViewL CommentId
                 -> Entity Comment
                 -> Tree (Entity Comment)
                 -> Tree (Entity Comment)
        addChild EmptyL    comment (Node x xs) = Node x (Tree.singleton comment : xs)
        addChild (b :< bs) comment (Node x xs) = Node x (addChild' b bs comment xs)

        addChild' :: CommentId
                  -> Seq CommentId
                  -> Entity Comment
                  -> Forest (Entity Comment)
                  -> Forest (Entity Comment)
        addChild' _ _ _ [] = error "Couldn't find forest to insert comment into."
        addChild' b bs comment (tree@(Node (Entity root_id _) _) : trees)
            | b == root_id = addChild (Seq.viewl bs) comment tree : trees
            | otherwise    = tree : addChild' b bs comment trees

        -- Order comment trees by newest-first, taking the root and all children of each
        -- tree into consideration (essentially compares each tree's newest comment,
        -- no matter how deeply nested)
        orderingNewestFirst :: Tree (Entity Comment) -> Tree (Entity Comment) -> Ordering
        orderingNewestFirst = flip (compare `on` (timestamp . newest))
          where
            newest :: Tree (Entity Comment) -> Entity Comment
            newest = L.maximumBy (compare `on` timestamp) . Tree.flatten

            timestamp :: Entity Comment -> UTCTime
            timestamp = commentTimestamp . entityVal

postResourceCommentsR :: ResourceId -> Handler Html
postResourceCommentsR = undefined
