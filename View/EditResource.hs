module View.EditResource 
    ( editResourceForm 
    ) where

import Import

import View.Resource (resourceTagsForm, resourceTypeField)

editResourceForm :: Maybe Text          -- default title
                 -> Maybe ResourceType  -- default type
                 -> Maybe (Set Text)    -- default tags
                 -> Form (Text, ResourceType, Set Text)
editResourceForm title typ tags = renderDivs $ (,,)
    <$> areq textField         "Title" title
    <*> areq resourceTypeField "Type"  typ
    <*> resourceTagsForm tags
