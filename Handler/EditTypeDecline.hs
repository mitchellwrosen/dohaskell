module Handler.EditTypeDecline where

import Import

import Handler.Utils (editDecline)

postEditTypeDeclineR :: EditTypeId -> Handler Html
postEditTypeDeclineR = editDecline editTypeResId
