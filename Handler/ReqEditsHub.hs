module Handler.ReqEditsHub where

import Import

import Handler.Utils  (denyPermissionIfDifferentUser)
import Model.Resource (getResourcesWithPendingEditsForUser)
import View.Navbar    (navbarWidget)

getReqEditsHubR :: UserId -> Handler Html
getReqEditsHubR uid = do
    denyPermissionIfDifferentUser uid
    results <- runDB $ getResourcesWithPendingEditsForUser uid
    defaultLayout $ do
        setTitle "Requested Edits"
        $(widgetFile "requested-edits-hub")
