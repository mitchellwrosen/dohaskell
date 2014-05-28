module Model.PendingResourceEdit
    ( getPendingEdits
    , getPendingEditTags
    , getPendingEditWithResource
    , insertPendingResourceEdit
    ) where

import Import

getPendingEdits :: UserId -> ResourceId -> YesodDB App [Entity PendingResourceEdit]
getPendingEdits uid resId =
    select $ 
        from $ \(u `InnerJoin` r `InnerJoin` e) -> do
        on (u^.UserId ==. r^.ResourceUserId)
        on (r^.ResourceId ==. e^.PendingResourceEditResourceId)
        where_ (u^.UserId ==. val uid &&. r^.ResourceId ==. val resId)
        return e

getPendingEditTags :: PendingResourceEditId -> YesodDB App [Text]
getPendingEditTags eid = map unValue <$> go
  where
    go = select $
            from $ \et -> do
            where_ (et^.PendingResourceEditTagEditId ==. val eid)
            return (et^.PendingResourceEditTagTag)

getPendingEditWithResource :: PendingResourceEditId -> Handler (PendingResourceEdit, Resource)
getPendingEditWithResource eid = do
    edit <- runDB $ get404 eid
    res  <- runDB $ get404 (pendingResourceEditResourceId edit)
    return (edit, res)

insertPendingResourceEdit :: ResourceId -> Text -> ResourceType -> [Tag] -> YesodDB App ()
insertPendingResourceEdit resId title typ tags = do
    -- insertUnique because someone may input the same tag twice. Don't blow up,
    -- just ignore it.
    editId <- insertBy' $ PendingResourceEdit resId title typ
    mapM_ (insertUnique . PendingResourceEditTag editId . tagText) tags
