{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import Model.Comment  (makeComment)
import Model.Resource (getResourceComments)
import Model.User     (unsafeGetUserById)
import View.Navbar    (navbarWidget)
import View.Resource  (resourceWidget)

getResourceR :: ResourceId -> Handler Html
getResourceR resId = do
    comments <- map entityVal <$> runDB (getResourceComments resId)

    (widget, enctype) <- generateFormPost commentForm
    defaultLayout $ do
        setTitle "Resource"
        $(widgetFile "resource")

commentWidget :: Comment -> Widget
commentWidget Comment{..} = do
    user <- handlerToWidget $ unsafeGetUserById commentUserId
    [whamlet|
        <li>
            <a href=@{UserR commentUserId}>#{maybe "(none)" id $ userDisplayName user}</a> (#{show $ commentPosted}): #{commentText}
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
commentForm = renderDivs $ areq textareaField "Submit comment:" Nothing
