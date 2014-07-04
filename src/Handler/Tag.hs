module Handler.Tag where

import Import

import Data.List      (sortBy)
import Text.Hamlet    (hamletFile)

import Handler.Utils  (alphabeticIgnoreCase)
import Model.Resource (getResourcesWithTag)
import View.Resource  (resourceListWidget)

getTagR :: Text -> Handler Html
getTagR text = do
    resources <- sortBy (alphabeticIgnoreCase resourceTitle) <$>
                     runDB (getResourcesWithTag text)
    is_embed  <- maybe False (const True) <$> lookupGetParam "embed"
    let body = resourceListWidget resources ("dohaskell | " <> text)
    if is_embed
      then do
          pc <- widgetToPageContent body
          giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
      else defaultLayout body
