module Handler.EditRemoveTagAccept where

import Import

import qualified Database.Persist as P

import Handler.Utils (editAccept)

postEditRemoveTagAcceptR :: EditRemoveTagId -> Handler Html
postEditRemoveTagAcceptR = editAccept editRemoveTagResId sqlCode
  where
    sqlCode (EditRemoveTag resId text) = do
        getBy (UniqueTagText text) >>= \case
            Nothing -> return ()
            Just (Entity tagId _) -> do
                -- Delete the resource-tag relationship, and possibly also 
                -- delete the tag (if no other resources share it)
                deleteBy $ UniqueResourceTag resId tagId
                n <- P.count [ResourceTagTagId P.==. tagId]
                when (n == 0) $
                    P.delete tagId
