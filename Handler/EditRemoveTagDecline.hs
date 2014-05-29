module Handler.EditRemoveTagDecline where

import Import

import Handler.Utils (editDecline)

postEditRemoveTagDeclineR :: EditRemoveTagId -> Handler Html
postEditRemoveTagDeclineR = editDecline editRemoveTagResId
