module View.EditResource 
    ( editResourceForm 
    ) where

import Import

import View.Resource (resourceTagsForm, resourceTypeField)

editResourceForm :: Maybe Text          -- default title
                 -> Maybe (Maybe Text)  -- default author
                 -> Maybe ResourceType  -- default type
                 -> Maybe (Set Text)    -- default tags
                 -> Form (Text, Maybe Text, ResourceType, Set Text)
editResourceForm title author typ tags = renderDivs $ (,,,)
    <$> areq textField         "Title"  title
    <*> aopt textField         "Primary Author (optional)" author
    <*> areq resourceTypeField "Type"   typ
    <*> resourceTagsForm tags
