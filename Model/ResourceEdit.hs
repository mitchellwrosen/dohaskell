module Model.ResourceEdit 
    ( getAllEditAuthors
    , getAllEditTitles
    , getAllEditTypes
    , getAllEditAddTags
    , getAllEditRemoveTags
    , getEditAuthors
    , getEditTitles
    , getEditTypes
    , getEditAddTags
    , getEditRemoveTags
    , getNumRequestedEdits
    ) where

import Import

import qualified Data.Map as M

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

getEditTitles :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditTitle])
getEditTitles = getEdit EditTitleResId

getAllEditTitles :: YesodDB App (Map (Entity Resource) [Entity EditTitle])
getAllEditTitles = getAllEdits EditTitleResId

getEditAuthors :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAuthor])
getEditAuthors = getEdit EditAuthorResId

getAllEditAuthors :: YesodDB App (Map (Entity Resource) [Entity EditAuthor])
getAllEditAuthors = getAllEdits EditAuthorResId

getEditTypes :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditType])
getEditTypes = getEdit EditTypeResId

getAllEditTypes :: YesodDB App (Map (Entity Resource) [Entity EditType])
getAllEditTypes = getAllEdits EditTypeResId

getEditAddTags :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditAddTag])
getEditAddTags = getEdit EditAddTagResId

getAllEditAddTags :: YesodDB App (Map (Entity Resource) [Entity EditAddTag])
getAllEditAddTags = getAllEdits EditAddTagResId

getEditRemoveTags :: UserId -> YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])
getEditRemoveTags = getEdit EditRemoveTagResId

getAllEditRemoveTags :: YesodDB App (Map (Entity Resource) [Entity EditRemoveTag])
getAllEditRemoveTags = getAllEdits EditRemoveTagResId

-- TODO: Should probably select count(*) ?
getNumRequestedEdits :: UserId -> YesodDB App Int
getNumRequestedEdits uid = do
    a <- adjust <$> getEditTitles uid
    b <- adjust <$> getEditTypes uid
    c <- adjust <$> getEditAddTags uid
    d <- adjust <$> getEditRemoveTags uid
    return $ a + b + c + d
  where
    adjust :: Map k [a] -> Int
    adjust = length . concat . M.elems
