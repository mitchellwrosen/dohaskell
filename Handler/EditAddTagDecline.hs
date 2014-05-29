module Handler.EditAddTagDecline where

import Import

import Handler.Utils (editDecline)

postEditAddTagDeclineR :: EditAddTagId -> Handler Html
postEditAddTagDeclineR = editDecline editAddTagResId
