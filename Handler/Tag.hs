module Handler.Tag where

import Import

import Text.Hamlet    (hamletFile)

import Model.Resource (getResourcesWithTag)
import View.Resource  (resourceListWidget)

getTagR :: Text -> Handler Html
getTagR text = do
    resources <- runDB $ getResourcesWithTag text
    is_embed  <- maybe False (const True) <$> lookupGetParam "embed"
    let body = resourceListWidget resources ("dohaskell | " <> text)
    if is_embed
      then do
          pc <- widgetToPageContent $ do
              $(combineStylesheets 'StaticR
                  [ css_normalize_css
                  , css_bootstrap_css
                  ])
              body
          giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
      else defaultLayout body
