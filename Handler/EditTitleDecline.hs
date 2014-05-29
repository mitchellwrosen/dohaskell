module Handler.EditTitleDecline where

import Import

import Handler.Utils (editDecline)

postEditTitleDeclineR :: EditTitleId -> Handler Html
postEditTitleDeclineR = editDecline editTitleResId
