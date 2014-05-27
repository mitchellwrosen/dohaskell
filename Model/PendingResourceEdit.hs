module Model.PendingResourceEdit
    ( insertPendingResourceEdit
    ) where

import Import

insertPendingResourceEdit :: ResourceId -> Text -> ResourceType -> [Tag] -> YesodDB App ()
insertPendingResourceEdit resId title typ tags = do
    -- insertUnique because someone may input the same tag twice. Don't blow up,
    -- just ignore it.
    editId <- insertBy' $ PendingResourceEdit resId title typ
    mapM_ (insertUnique . PendingResourceEditTag editId . tagText) tags
