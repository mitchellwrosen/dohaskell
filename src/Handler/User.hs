module Handler.User where

import Import

import           Handler.Utils      (denyPermissionIfDifferentUser)
import           Model.List
import           Model.ResourceEdit (fetchNumRequestedEditsDB)
import           Model.User
import           Model.Utils
import           View.Browse
import           View.User

import qualified Data.Text as T

plural :: Int -> Text -> Text
plural 1 = id
plural _ = flip T.snoc 's'

getUserR :: UserId -> Handler Html
getUserR user_id = do
    user <- runDB $ get404 user_id
    (widget, enctype) <- generateFormPost (displayNameForm . Just $ userDisplayName user)

    is_own_profile <- maybe False (== user_id) <$> maybeAuthId
    (num_req_edits, num_submitted, num_grokked) <- runDB $ (,,)
        <$> (if is_own_profile then fetchNumRequestedEditsDB user_id else return 0) -- bogus val, not used in html
        <*> fetchNumSubmittedResourcesDB user_id
        <*> fetchNumGrokkedResourcesDB   user_id

    defaultLayout $ do
        setTitle "dohaskell | profile"
        $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR user_id = do
    denyPermissionIfDifferentUser user_id
    ((result, _), _) <- runFormPost (displayNameForm Nothing)
    case result of
        FormSuccess displayName -> do
            runDB (updateUserDisplayNameDB user_id displayName)
            setMessage "Display name updated."
            redirect $ UserR user_id
        FormFailure err -> userFormFailure ("Form failure: " <> T.intercalate "," err)
        FormMissing     -> userFormFailure ("Form missing")
  where
    userFormFailure :: Text -> Handler a
    userFormFailure msg = do
        setMessage (toHtml msg)
        redirect (UserR user_id)

getUserSubmittedR :: UserId -> Handler Html
getUserSubmittedR user_id = do
    (display_name, unsorted_resources) <- runDB $ (,)
        <$> (userDisplayName <$> get404 user_id)
        <*> fetchSubmittedResourcesDB user_id

    let resources = sortBy (orderAlphabeticIgnoreCase (resourceTitle . entityVal)) unsorted_resources

    defaultLayout $ do
      setTitle . toHtml $ "dohaskell | submitted by " <> display_name
      resourceListWidget resources

getUserListR :: UserId -> Text -> Handler Html
getUserListR user_id list_name =
    runDB (fetchListResourcesDB user_id list_name)
      >>= defaultLayout . resourceListWidget
