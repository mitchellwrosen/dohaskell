module Handler.EditHub where

import Import

import Handler.Utils (denyPermissionIfDifferentUser)
import View.Navbar   (navbarWidget)

getEditHubR :: UserId -> Handler Html
getEditHubR uid = do
    denyPermissionIfDifferentUser uid
    defaultLayout $ do
        setTitle "Edit Hub"
        $(widgetFile "edit-hub")
