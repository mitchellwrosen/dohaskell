module Handler.ReqEdits where

import Import

import Handler.Utils             (denyPermissionIfDifferentUser)
import Model.PendingResourceEdit (getPendingEdits)
import View.Navbar               (navbarWidget)
import View.PendingResourceEdit  (pendingResourceEditWidget)
import View.Resource             (resourceWidget)

getReqEditsR :: UserId -> ResourceId -> Handler Html
getReqEditsR uid resId = do
    denyPermissionIfDifferentUser uid
    edits <- runDB $ getPendingEdits uid resId
    defaultLayout $ do
        setTitle $ "Requested Edits"
        $(widgetFile "requested-edits")
