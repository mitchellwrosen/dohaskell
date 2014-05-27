module Handler.About where

import Import

import View.Navbar

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitle "About Dohaskell.com"
    $(widgetFile "about")
