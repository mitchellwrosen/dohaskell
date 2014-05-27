module Handler.Home where

import Import

import View.Navbar (navbarWidget)

getHomeR :: Handler Html
getHomeR = do
    tags <- runDB $ selectList [] [Asc TagText]
    defaultLayout $ do
        setTitle "Dohaskell.com: Tagged Haskell learning resources"
        $(widgetFile "homepage")
