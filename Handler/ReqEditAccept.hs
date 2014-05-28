module Handler.ReqEditAccept where

import Import

import qualified Database.Persist

import Model.Resource            (updateResource)
import Model.PendingResourceEdit (getPendingEditTags, getPendingEditWithResource)
import Handler.Utils             (denyPermissionIfDoesntHaveAuthorityOver)

postReqEditAcceptR :: PendingResourceEditId -> Handler Html
postReqEditAcceptR eid = do
    (PendingResourceEdit resId title typ, res) <- getPendingEditWithResource eid
    let uid = resourceUserId res

    -- Admins and the user himself may accept an edit.
    denyPermissionIfDoesntHaveAuthorityOver uid

    tags <- map Tag <$> runDB (getPendingEditTags eid)
    runDB $ updateResource resId title typ tags

    runDB $ Database.Persist.delete eid
    redirect $ ReqEditsR uid resId
