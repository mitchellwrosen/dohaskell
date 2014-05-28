module Handler.ReqEditDecline where

import Import

import qualified Database.Persist

import Handler.Utils             (denyPermissionIfDoesntHaveAuthorityOver)
import Model.PendingResourceEdit (getPendingEditWithResource)

postReqEditDeclineR :: PendingResourceEditId -> Handler Html
postReqEditDeclineR eid = do
    (PendingResourceEdit resId _ _, res) <- getPendingEditWithResource eid
    let uid = resourceUserId res

    -- Admins and the user himself may decline an edit.
    denyPermissionIfDoesntHaveAuthorityOver uid
    runDB $ Database.Persist.delete eid
    redirect $ ReqEditsR uid resId
