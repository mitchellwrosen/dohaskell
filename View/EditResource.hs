module View.EditResource
    ( editResourceForm
    ) where

import Import

import View.Resource (resourceTagsForm, resourceTypeField)

editResourceForm :: Maybe Text          -- default title
                 -> Maybe (Maybe Text)  -- default author
                 -> Maybe (Maybe Int)   -- default published
                 -> Maybe ResourceType  -- default type
                 -> Maybe (Set Text)    -- default tags
                 -> Form (Text, Maybe Text, Maybe Int, ResourceType, Set Text)
editResourceForm title author published typ tags = renderDivs $ (,,,,)
    <$> areq textField         "Title"                     title
    <*> aopt textField         "Primary Author (optional)" author
    <*> aopt intField          "Year (optional)"           published
    <*> areq resourceTypeField "Type"                      typ
    <*> resourceTagsForm tags
