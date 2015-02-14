module Handler.FayCommand where

import Import

import SharedTypes

import Fay.Convert (readFromFay)
import Yesod.Fay

fayCommandHandler :: CommandHandler App
fayCommandHandler render command =
    case readFromFay command of
        Just (RollDie r) -> render r "Four"
        Nothing          -> invalidArgs ["Invalid command"]
