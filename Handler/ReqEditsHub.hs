module Handler.ReqEditsHub where

import Import

import Handler.Utils      (denyPermissionIfDifferentUser)
import Model.ResourceEdit
import View.ResourceEdit

getReqEditsHubR :: UserId -> Handler Html
getReqEditsHubR uid = do
    denyPermissionIfDifferentUser uid
    editTitles     <- runDB $ getEditTitles     uid
    editTypes      <- runDB $ getEditTypes      uid
    editAddTags    <- runDB $ getEditAddTags    uid
    editRemoveTags <- runDB $ getEditRemoveTags uid
    defaultLayout $ do
        setTitle "Requested Edits"
        $(widgetFile "requested-edits-hub")
