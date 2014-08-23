module Model.ResourceEdit
    ( fetchAllEditAddTagsDB
    , fetchAllEditAuthorsDB
    , fetchAllEditPublishedDB
    , fetchAllEditRemoveTagsDB
    , fetchAllEditTitlesDB
    , fetchAllEditTypesDB
    , fetchEditAddTagsDB
    , fetchEditAuthorsDB
    , fetchEditPublishedDB
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

fetchEditTitlesDB        :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditTitle])
fetchEditAuthorsDB       :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAuthors])
fetchEditPublishedDB     :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditPublished])
fetchEditTypesDB         :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditType])
fetchEditAddTagsDB       :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAddTag])
fetchEditRemoveTagsDB    :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])

fetchEditTitlesDB     = fetchEditDB EditTitleResId
fetchEditAuthorsDB    = fetchEditDB EditAuthorsResId
fetchEditPublishedDB  = fetchEditDB EditPublishedResId
fetchEditTypesDB      = fetchEditDB EditTypeResId
fetchEditAddTagsDB    = fetchEditDB EditAddTagResId
fetchEditRemoveTagsDB = fetchEditDB EditRemoveTagResId

fetchAllEditTitlesDB     :: YesodDB App (Map (Entity Resource) [Entity EditTitle])
fetchAllEditAuthorsDB    :: YesodDB App (Map (Entity Resource) [Entity EditAuthors])
fetchAllEditPublishedDB  :: YesodDB App (Map (Entity Resource) [Entity EditPublished])
fetchAllEditTypesDB      :: YesodDB App (Map (Entity Resource) [Entity EditType])
fetchAllEditAddTagsDB    :: YesodDB App (Map (Entity Resource) [Entity EditAddTag])
fetchAllEditRemoveTagsDB :: YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])

fetchAllEditTitlesDB     = fetchAllEditsDB EditTitleResId
fetchAllEditAuthorsDB    = fetchAllEditsDB EditAuthorsResId
fetchAllEditPublishedDB  = fetchAllEditsDB EditPublishedResId
fetchAllEditTypesDB      = fetchAllEditsDB EditTypeResId
fetchAllEditAddTagsDB    = fetchAllEditsDB EditAddTagResId
fetchAllEditRemoveTagsDB = fetchAllEditsDB EditRemoveTagResId

-- TODO: Should probably select count(*) ?
fetchNumRequestedEditsDB :: UserId -> YesodDB App Int
fetchNumRequestedEditsDB uid = getSum . mconcat <$>
    sequence
        [ adjust <$> fetchEditTitlesDB     uid
        , adjust <$> fetchEditAuthorsDB    uid
        , adjust <$> fetchEditPublishedDB  uid
        , adjust <$> fetchEditTypesDB      uid
        , adjust <$> fetchEditAddTagsDB    uid
        , adjust <$> fetchEditRemoveTagsDB uid
        ]
  where
    adjust :: Map k [a] -> Sum Int
    adjust = F.foldMap (Sum . length)
