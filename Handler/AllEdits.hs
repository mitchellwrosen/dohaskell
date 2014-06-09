module Handler.AllEdits where

import Import

import Handler.Utils      (denyPermissionIfNotAdmin)
import Model.ResourceEdit
import View.ResourceEdit

getAllEditsR :: Handler Html
getAllEditsR = do
    denyPermissionIfNotAdmin
    {-editTitles     <- runDB getAllEditTitles-}
    {-editTypes      <- runDB getAllEditTypes-}
    {-editAddTags    <- runDB getAllEditAddTags-}
    {-editRemoveTags <- runDB getAllEditRemoveTags-}
    {-let areNoRequestedEdits = null editTitles-}
                           {-&& null editTypes-}
                           {-&& null editAddTags-}
                           {-&& null editRemoveTags-}

    {-setUltDestCurrent-}
    {-defaultLayout $ do-}
        {-setTitle "dohaskell | requested edits"-}
        {-$(widgetFile "requested-edits-hub")-}
    setMessage "TODO"
    redirect HomeR
