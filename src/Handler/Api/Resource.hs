module Handler.Api.Resource where

import Import

import Data.Aeson

postApiResourceExists :: Handler TypedContent
postApiResourceExists = return $ toTypedContent $
    object
        [ "success" .= True
        , "result"  .= True
        ]
