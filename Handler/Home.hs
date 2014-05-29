module Handler.Home where

import Import

import View.Navbar (navbarWidget)

getHomeR :: Handler Html
getHomeR = do
    tags <- runDB $
        map unValue <$>
            (select $
                from $ \(t `InnerJoin` rt) -> do
                on (t^.TagId ==. rt^.ResourceTagTagId)
                orderBy [asc (t^.TagText)]
                return (t^.TagText))
    defaultLayout $ do
        setTitle "Dohaskell.com: Tagged Haskell learning resources"
        $(widgetFile "homepage")
