module Handler.AuthorAndTag
    ( getAuthorR
    , getTagR
    ) where

import Import

import Handler.Utils
import Model.Resource
import View.Browse

import Data.List   (sortBy)
import Data.Maybe  (isJust)
import Text.Blaze  (ToMarkup)
import Text.Hamlet (hamletFile)

getAuthorR, getTagR :: Text -> Handler Html
getAuthorR text = getResources (getResourcesWithAuthor text) ("dohaskell | by " <> text)
getTagR    text = getResources (getResourcesWithTag text)    ("dohaskell | " <> text)

-- | Abstract both getAuthorR and getTagR. Assumes the resources grabbed from the
-- database are unsorted.
getResources :: ToMarkup markup => YesodDB App [Entity Resource] -> markup -> Handler Html
getResources get_resources title = do
    resources <- sortBy (alphabeticIgnoreCase resourceTitle) <$> runDB get_resources
    is_embed  <- isJust <$> lookupGetParam "embed"
    let body = do
          setTitle (toHtml title)
          resourceListWidget resources
    if is_embed
      then do
          pc <- widgetToPageContent body
          giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
      else defaultLayout body
