module Model.ResourceEdit 
    ( getEditTitles
    , getEditTypes
    , getEditAddTags
    , getEditRemoveTags
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

getEditTitles :: UserId -> YesodDB App [(Entity Resource, Entity EditTitle)]
getEditTitles = getEdit EditTitleResId

getEditTypes :: UserId -> YesodDB App [(Entity Resource, Entity EditType)]
getEditTypes = getEdit EditTypeResId

getEditAddTags :: UserId -> YesodDB App [(Entity Resource, Entity EditAddTag)]
getEditAddTags = getEdit EditAddTagResId

getEditRemoveTags :: UserId -> YesodDB App [(Entity Resource, Entity EditRemoveTag)]
getEditRemoveTags = getEdit EditRemoveTagResId
