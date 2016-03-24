module Handler.Api.Resource where

import Import

postApiResourceExists :: Handler TypedContent
postApiResourceExists = return $ toTypedContent $
    object
        [ "success" .= True
        , "result"  .= True
        ]
