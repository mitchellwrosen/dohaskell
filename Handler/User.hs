module Handler.User where

import Import

import           Data.Char     (isAlphaNum)
import qualified Data.Text     as T

import           Handler.Utils (denyPermissionIfDifferentUser)
import           Model.User
import           View.Navbar   (navbarWidget)

getUserR :: UserId -> Handler Html
getUserR uid = do
    user <- runDB $ get404 uid
    (widget, enctype) <- generateFormPost (displayNameForm $ userDisplayName user)

    -- Is the user looking at their own profile?
    isOwnProfile <- maybe False (== uid) <$> maybeAuthId

    defaultLayout $ do
        setTitle "User"
        $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
    denyPermissionIfDifferentUser uid
    ((result, _), _) <- runFormPost (displayNameForm Nothing)
    case result of
        FormSuccess displayName -> do
            updateUserDisplayName uid displayName
            setMessage "Display name updated."
            redirect $ UserR uid
        FormFailure err -> do
            setMessage (toHtml $ "Form failure: " <> T.intercalate "," err)
            redirect (UserR uid)
        FormMissing -> redirect (UserR uid)

displayNameForm :: Maybe Text -> Form Text
displayNameForm curDisplayName = renderDivs $ areq
    (checkBool validName ("Only alphanumeric characters allowed."::Text) textField)
    "Set display name:"
    curDisplayName
  where
    validName :: Text -> Bool
    validName = allCharsSatisfy isAlphaNum

    allCharsSatisfy :: (Char -> Bool) -> Text -> Bool
    allCharsSatisfy f = T.foldr (\c b -> f c && b) True
