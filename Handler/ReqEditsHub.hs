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
    let areNoRequestedEdits = null editTitles
                           && null editTypes
                           && null editAddTags
                           && null editRemoveTags
    defaultLayout $ do
        setTitle "dohaskell | requested edits"
        $(widgetFile "requested-edits-hub")
