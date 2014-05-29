module Handler.EditTitleAccept where

import Import

import Handler.Utils (editAccept)

postEditTitleAcceptR :: EditTitleId -> Handler Html
postEditTitleAcceptR = editAccept editTitleResId sqlCode
  where
    sqlCode (EditTitle resId title) =
        update $ \r -> do
        set r [ ResourceTitle =. val title ]
        where_ (r^.ResourceId ==. val resId)
