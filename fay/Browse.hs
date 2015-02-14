module Browse where

import Prelude

import DOM
import JQuery

main :: Fay ()
main = getDocument >>= documentReady (const main')

main' :: Fay ()
-- main' = select ".expand" >>= onClick (\_ -> call RollDie alert >> return False)
main' = select ".expand" >>= onClick toggleExpand
    where
        toggleExpand :: JQuery -> Fay JQuery
        toggleExpand elem = do
            expanded <- hasClass "exp" elem
            if expanded
                then removeClass "exp" elem >>= collapse
                else addClass    "exp" elem >>= expand

        collapse :: JQuery -> Fay JQuery
        collapse elem = resourcesContainer elem >>= hide Instantly

        expand :: JQuery -> Fay JQuery
        expand elem = do
            res_container <- unhide =<< resourcesContainer elem
            fetched <- hasClass "fetched" res_container
            unless fetched $ do
                url <- permalink elem
                load (\_ -> addClass "fetched" res_container) res_container

resourcesContainer :: JQuery -> Fay JQuery
resourcesContainer = siblingsSelector ".res-container"

permalink :: JQuery -> Fay String
permalink elem = siblingsSelector ".permalink" >>= getText
