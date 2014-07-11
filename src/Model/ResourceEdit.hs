module Model.ResourceEdit
    ( getAllEditAddTags
    , getAllEditAuthors
    , getAllEditPublished
    , getAllEditRemoveTags
    , getAllEditTitles
    , getAllEditTypes
    , getEditAddTags
    , getEditAuthors
    , getEditPublished
    , getEditRemoveTags
    , getEditTitles
    , getEditTypes
    , getNumRequestedEdits
    ) where

import Import

import qualified Data.Map as M
import           Database.Esqueleto
import           Data.Monoid (Sum(..), getSum)

-- TODO: Do we really have to do this 3-table join for each kind of edit...
getEdit :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
        => EntityField val ResourceId
        -> UserId
        -> YesodDB App (Map (Entity Resource) [Entity val])
getEdit resIdField uid = fmap makeEditMap $
    select $
        from $ \(u `InnerJoin` r `InnerJoin` e) -> do
        on (u^.UserId     ==. r^.ResourceUserId)
        on (r^.ResourceId ==. e^.resIdField)
        where_ (u^.UserId ==. val uid)
        return (r,e)

getAllEdits :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => EntityField val ResourceId
            -> YesodDB App (Map (Entity Resource) [Entity val])
getAllEdits resIdField = fmap makeEditMap $
    select $
        from $ \(r `InnerJoin` e) -> do
        on (r^.ResourceId ==. e^.resIdField)
        return (r,e)

-- Not quite M.fromListWith, but close.
makeEditMap :: Ord k => [(k,a)] -> Map k [a]
makeEditMap = foldr (\(k,a) -> M.insertWith (++) k [a]) M.empty

getEditTitles        :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditTitle])
getEditAuthors       :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAuthors])
getEditPublished     :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditPublished])
getEditTypes         :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditType])
getEditAddTags       :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAddTag])
getEditRemoveTags    :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])

getEditTitles     = getEdit EditTitleResId
getEditAuthors    = getEdit EditAuthorsResId
getEditPublished  = getEdit EditPublishedResId
getEditTypes      = getEdit EditTypeResId
getEditAddTags    = getEdit EditAddTagResId
getEditRemoveTags = getEdit EditRemoveTagResId

getAllEditTitles     :: YesodDB App (Map (Entity Resource) [Entity EditTitle])
getAllEditAuthors    :: YesodDB App (Map (Entity Resource) [Entity EditAuthors])
getAllEditPublished  :: YesodDB App (Map (Entity Resource) [Entity EditPublished])
getAllEditTypes      :: YesodDB App (Map (Entity Resource) [Entity EditType])
getAllEditAddTags    :: YesodDB App (Map (Entity Resource) [Entity EditAddTag])
getAllEditRemoveTags :: YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])

getAllEditTitles     = getAllEdits EditTitleResId
getAllEditAuthors    = getAllEdits EditAuthorsResId
getAllEditPublished  = getAllEdits EditPublishedResId
getAllEditTypes      = getAllEdits EditTypeResId
getAllEditAddTags    = getAllEdits EditAddTagResId
getAllEditRemoveTags = getAllEdits EditRemoveTagResId

-- TODO: Should probably select count(*) ?
getNumRequestedEdits :: UserId -> YesodDB App Int
getNumRequestedEdits uid = do
    getSum . mconcat <$>
        sequence
            [ adjust <$> getEditTitles     uid
            , adjust <$> getEditAuthors    uid
            , adjust <$> getEditPublished  uid
            , adjust <$> getEditTypes      uid
            , adjust <$> getEditAddTags    uid
            , adjust <$> getEditRemoveTags uid
            ]
  where
    adjust :: Map k [a] -> Sum Int
    adjust = Sum . length . concat . M.elems
