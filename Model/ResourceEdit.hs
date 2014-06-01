module Model.ResourceEdit 
    ( getAllEditTitles
    , getAllEditTypes
    , getAllEditAddTags
    , getAllEditRemoveTags
    , getEditTitles
    , getEditTypes
    , getEditAddTags
    , getEditRemoveTags
    , getNumRequestedEdits
    ) where

import Import

-- TODO: Do we really have to do this 3-table join for each kind of edit...
getEdit :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
        => EntityField val ResourceId
        -> UserId
        -> YesodDB App [(Entity Resource, Entity val)]
getEdit resIdField uid = do
    select $
        from $ \(u `InnerJoin` r `InnerJoin` e) -> do
        on (u^.UserId     ==. r^.ResourceUserId)
        on (r^.ResourceId ==. e^.resIdField)
        where_ (u^.UserId ==. val uid)
        return (r,e)

getAllEdits :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => EntityField val ResourceId
            -> YesodDB App [(Entity Resource, Entity val)]
getAllEdits resIdField = do
    select $
        from $ \(r `InnerJoin` e) -> do
        on (r^.ResourceId ==. e^.resIdField)
        return (r,e)

getEditTitles :: UserId -> YesodDB App [(Entity Resource, Entity EditTitle)]
getEditTitles = getEdit EditTitleResId

getAllEditTitles :: YesodDB App [(Entity Resource, Entity EditTitle)]
getAllEditTitles = getAllEdits EditTitleResId

getEditTypes :: UserId -> YesodDB App [(Entity Resource, Entity EditType)]
getEditTypes = getEdit EditTypeResId

getAllEditTypes :: YesodDB App [(Entity Resource, Entity EditType)]
getAllEditTypes = getAllEdits EditTypeResId

getEditAddTags :: UserId -> YesodDB App [(Entity Resource, Entity EditAddTag)]
getEditAddTags = getEdit EditAddTagResId

getAllEditAddTags :: YesodDB App [(Entity Resource, Entity EditAddTag)]
getAllEditAddTags = getAllEdits EditAddTagResId

getEditRemoveTags :: UserId -> YesodDB App [(Entity Resource, Entity EditRemoveTag)]
getEditRemoveTags = getEdit EditRemoveTagResId

getAllEditRemoveTags :: YesodDB App [(Entity Resource, Entity EditRemoveTag)]
getAllEditRemoveTags = getAllEdits EditRemoveTagResId

-- TODO: Should probably select count(*)
getNumRequestedEdits :: UserId -> YesodDB App Int
getNumRequestedEdits uid = do
    a <- length <$> getEditTitles uid
    b <- length <$> getEditTypes uid
    c <- length <$> getEditAddTags uid
    d <- length <$> getEditRemoveTags uid
    return $ a + b + c + d
