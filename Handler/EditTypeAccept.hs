module Handler.EditTypeAccept where

import Import

import Handler.Utils (editAccept)

postEditTypeAcceptR :: EditTypeId -> Handler Html
postEditTypeAcceptR = editAccept editTypeResId sqlCode
  where
    sqlCode (EditType resId typ) =
        update $ \r -> do
        set r [ ResourceType =. val typ ]
        where_ (r^.ResourceId ==. val resId)
