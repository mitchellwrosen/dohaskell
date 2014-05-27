module Handler.Home where

import Import

import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    mauth <- maybeAuth
    tags <- runDB $ selectList [] [Asc TagTag]

    defaultLayout $ do
        setTitle "Dohaskell.com: Tagged Haskell learning resources"
        $(widgetFile "homepage")
