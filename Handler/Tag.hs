module Handler.Tag where

import Import

import Model.Resource (getResourcesWithTag)
import View.Resource (resourceListWidget)

getTagR :: Text -> Handler Html
getTagR text = do
    resources <- runDB $ getResourcesWithTag text
    defaultLayout $ resourceListWidget resources ("dohaskell | " <> text)
