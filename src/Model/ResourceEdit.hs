module Model.ResourceEdit
    ( fetchAllEditAddCollectionsDB
    , fetchAllEditAddTagsDB
    , fetchAllEditAuthorsDB
    , fetchAllEditPublishedDB
    , fetchAllEditRemoveCollectionsDB
    , fetchAllEditRemoveTagsDB
    , fetchAllEditTitlesDB
    , fetchAllEditTypesDB
    , fetchEditAddCollectionsDB
    , fetchEditAddTagsDB
    , fetchEditAuthorsDB
    , fetchEditPublishedDB
    , fetchEditRemoveCollectionsDB
    , fetchEditRemoveTagsDB
    , fetchEditTitlesDB
    , fetchEditTypesDB
    , fetchNumRequestedEditsDB
    ) where

import Import

import qualified Data.Foldable      as F
import qualified Data.Map           as M
import           Database.Esqueleto
import           Data.Monoid        (Sum(..), getSum)

fetchEditDB
        :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
        => EntityField val ResourceId
        -> UserId
        -> YesodDB App (Map (Entity Resource) [Entity val])
fetchEditDB resIdField uid = fmap makeEditMap $
    select $
    from $ \(u `InnerJoin` r `InnerJoin` e) -> do
    on (u^.UserId     ==. r^.ResourceUserId)
    on (r^.ResourceId ==. e^.resIdField)
    where_ (u^.UserId ==. val uid)
    return (r,e)

fetchAllEditsDB
        :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
        => EntityField val ResourceId
        -> YesodDB App (Map (Entity Resource) [Entity val])
fetchAllEditsDB resIdField = fmap makeEditMap $
    select $
    from $ \(r `InnerJoin` e) -> do
    on (r^.ResourceId ==. e^.resIdField)
    return (r,e)

-- Not quite M.fromListWith, but close.
makeEditMap :: Ord k => [(k,a)] -> Map k [a]
makeEditMap = foldr (\(k,a) -> M.insertWith (++) k [a]) M.empty

fetchEditAddCollectionsDB    :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAddCollection])
fetchEditAddTagsDB           :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAddTag])
fetchEditAuthorsDB           :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAuthors])
fetchEditPublishedDB         :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditPublished])
fetchEditRemoveCollectionsDB :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditRemoveCollection])
fetchEditRemoveTagsDB        :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])
fetchEditTitlesDB            :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditTitle])
fetchEditTypesDB             :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditType])

fetchEditAddCollectionsDB    = fetchEditDB EditAddCollectionResId
fetchEditAddTagsDB           = fetchEditDB EditAddTagResId
fetchEditAuthorsDB           = fetchEditDB EditAuthorsResId
fetchEditPublishedDB         = fetchEditDB EditPublishedResId
fetchEditRemoveCollectionsDB = fetchEditDB EditRemoveCollectionResId
fetchEditRemoveTagsDB        = fetchEditDB EditRemoveTagResId
fetchEditTitlesDB            = fetchEditDB EditTitleResId
fetchEditTypesDB             = fetchEditDB EditTypeResId

fetchAllEditAddCollectionsDB    :: YesodDB App (Map (Entity Resource) [Entity EditAddCollection])
fetchAllEditAddTagsDB           :: YesodDB App (Map (Entity Resource) [Entity EditAddTag])
fetchAllEditAuthorsDB           :: YesodDB App (Map (Entity Resource) [Entity EditAuthors])
fetchAllEditPublishedDB         :: YesodDB App (Map (Entity Resource) [Entity EditPublished])
fetchAllEditRemoveCollectionsDB :: YesodDB App (Map (Entity Resource) [Entity EditRemoveCollection])
fetchAllEditRemoveTagsDB        :: YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])
fetchAllEditTitlesDB            :: YesodDB App (Map (Entity Resource) [Entity EditTitle])
fetchAllEditTypesDB             :: YesodDB App (Map (Entity Resource) [Entity EditType])

fetchAllEditAddCollectionsDB    = fetchAllEditsDB EditAddCollectionResId
fetchAllEditAddTagsDB           = fetchAllEditsDB EditAddTagResId
fetchAllEditAuthorsDB           = fetchAllEditsDB EditAuthorsResId
fetchAllEditPublishedDB         = fetchAllEditsDB EditPublishedResId
fetchAllEditRemoveCollectionsDB = fetchAllEditsDB EditRemoveCollectionResId
fetchAllEditRemoveTagsDB        = fetchAllEditsDB EditRemoveTagResId
fetchAllEditTitlesDB            = fetchAllEditsDB EditTitleResId
fetchAllEditTypesDB             = fetchAllEditsDB EditTypeResId

-- TODO: Should probably select count(*) ?
fetchNumRequestedEditsDB :: UserId -> YesodDB App Int
fetchNumRequestedEditsDB uid = getSum . mconcat <$>
    sequence
        [ adjust <$> fetchEditAddCollectionsDB    uid
        , adjust <$> fetchEditAddTagsDB           uid
        , adjust <$> fetchEditAuthorsDB           uid
        , adjust <$> fetchEditPublishedDB         uid
        , adjust <$> fetchEditRemoveCollectionsDB uid
        , adjust <$> fetchEditRemoveTagsDB        uid
        , adjust <$> fetchEditTitlesDB            uid
        , adjust <$> fetchEditTypesDB             uid
        ]
  where
    adjust :: Map k [a] -> Sum Int
    adjust = F.foldMap (Sum . length)
