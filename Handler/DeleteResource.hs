module Handler.DeleteResource where

import Import

import Handler.Utils  (denyPermissionIfDoesntHaveAuthorityOver)
import Model.Resource (deleteResource)

postDeleteResourceR :: ResourceId -> Handler Html
postDeleteResourceR resId = do
    -- Admins and the user himself may delete a resource.
    (resourceUserId <$> runDB (get404 resId)) >>= denyPermissionIfDoesntHaveAuthorityOver

    runDB $ deleteResource resId
    setMessage "Resource deleted."
    redirect HomeR
