module Handler.Home where

import Import

import View.Navbar (navbarWidget)

getHomeR :: Handler Html
getHomeR = do
    -- rawSql here to get the COLLATE NOCASE. It would probably be smarter to 
    -- just do the sorting after the quary.
    tags <- map unSingle <$> runDB (rawSql "SELECT text FROM tag ORDER BY text COLLATE NOCASE ASC;" [])
    defaultLayout $ do
        setTitle "dohaskell: tagged Haskell learning resources"
        $(widgetFile "homepage")
