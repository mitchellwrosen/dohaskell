{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import Yesod.Auth     (requireAuthId)

import Model.Comment  (makeComment)
import Model.Resource
import Model.User     (unsafeGetUserById)

getResourceR :: ResourceId -> Handler Html
getResourceR resId = do
    res      <- runDB $ get404 resId
    user     <- unsafeGetUserById (resourceUser res)
    tags     <- runDB $ getResourceTags resId
    comments <- map entityVal <$> runDB (getResourceComments resId)

    (widget, enctype) <- generateFormPost commentForm
    defaultLayout $ do
        setTitle "Resource"
        $(widgetFile "resource")

commentWidget :: Comment -> Widget
commentWidget Comment{..} = do
    user <- handlerToWidget $ unsafeGetUserById commentUser
    [whamlet|
        <li>
            <a href=@{UserR commentUser}>#{maybe "(none)" id $ userDisplayName user}</a>: #{commentText}
    |]

postResourceR :: ResourceId -> Handler Html
postResourceR resId = do
    uid <- requireAuthId
    ((result, _), _) <- runFormPost commentForm
    case result of
        FormSuccess text -> do
            makeComment resId uid text
            redirect $ ResourceR resId
        _ -> do
            setMessage "Error submitting comment."
            redirect $ ResourceR resId

commentForm :: Form Textarea
commentForm = renderDivs $ areq textareaField "Make comment:" Nothing
