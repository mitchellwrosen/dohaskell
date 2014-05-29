module Handler.EditRemoveTagAccept where

import Import

import Handler.Utils (editAccept)

postEditRemoveTagAcceptR :: EditRemoveTagId -> Handler Html
postEditRemoveTagAcceptR = editAccept editRemoveTagResId sqlCode
  where
    sqlCode (EditRemoveTag resId text) = do
        getBy (UniqueTagText text) >>= \case
            Nothing -> return ()
            Just (Entity tagId _) -> do
                deleteBy $ UniqueResourceTag resId tagId
