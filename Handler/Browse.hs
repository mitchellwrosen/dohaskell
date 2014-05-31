module Handler.Browse where

import Import

getBrowseR :: Handler Html
getBrowseR = do
    -- TODO: Sort in Haskell rather than in database?
    resources <- runDB $ rawSql "SELECT * FROM resource ORDER BY title COLLATE NOCASE ASC;" []
    defaultLayout $ do
        setTitle "dohaskell | browse"
        $(widgetFile "resource-list")
