module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "dohaskell | about"
        $(widgetFile "about")
