module View.Navbar where

import Import

navbarWidget :: Widget
navbarWidget = do
    muid <- handlerToWidget maybeAuthId
    $(widgetFile "navbar")
