{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    mauth <- maybeAuth
    tags <- runDB $ selectList [] [Desc TagTag]

    defaultLayout $ do
        setTitle "Hi"
        $(widgetFile "homepage")
