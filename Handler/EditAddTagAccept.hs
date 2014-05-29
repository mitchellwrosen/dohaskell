module Handler.EditAddTagAccept where

import Import

import Handler.Utils (editAccept)

postEditAddTagAcceptR :: EditAddTagId -> Handler Html
postEditAddTagAcceptR = editAccept editAddTagResId sqlCode
  where
    sqlCode (EditAddTag resId text) = do
        tagId <- insertBy' (Tag text)
        void . insertUnique $ ResourceTag resId tagId
