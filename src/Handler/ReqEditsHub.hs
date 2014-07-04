module Handler.ReqEditsHub where

import Import

import           Handler.Utils      (denyPermissionIfDifferentUser, denyPermissionIfNotAdmin)
import           Model.ResourceEdit
import           View.Resource      (resourceInfoWidget)

import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T

getReqEditsHubR :: UserId -> Handler Html
getReqEditsHubR uid = do
    denyPermissionIfDifferentUser uid
    runDB ((,,,,)
        <$> getEditTitles     uid
        <*> getEditAuthors    uid
        <*> getEditTypes      uid
        <*> getEditAddTags    uid
        <*> getEditRemoveTags uid)
            >>= getRequestedEdits

getAllEditsR :: Handler Html
getAllEditsR = do
    denyPermissionIfNotAdmin
    runDB ((,,,,)
        <$> getAllEditTitles
        <*> getAllEditAuthors
        <*> getAllEditTypes
        <*> getAllEditAddTags
        <*> getAllEditRemoveTags)
            >>= getRequestedEdits

getRequestedEdits :: ( Map (Entity Resource) [Entity EditTitle]
                     , Map (Entity Resource) [Entity EditAuthors]
                     , Map (Entity Resource) [Entity EditType]
                     , Map (Entity Resource) [Entity EditAddTag]
                     , Map (Entity Resource) [Entity EditRemoveTag]
                     )
                  -> Handler Html
getRequestedEdits (editTitles, editAuthors, editTypes, editAddTags, editRemoveTags) = do
    let areNoRequestedEdits :: Bool
        areNoRequestedEdits =
            M.null editTitles  &&
            M.null editAuthors &&
            M.null editTypes   &&
            M.null editAddTags &&
            M.null editRemoveTags

        resources :: Set (Entity Resource)
        resources =
            S.fromList (M.keys editTitles)  <>
            S.fromList (M.keys editAuthors) <>
            S.fromList (M.keys editTypes)   <>
            S.fromList (M.keys editAddTags) <>
            S.fromList (M.keys editRemoveTags)

    setUltDestCurrent
    defaultLayout $ do
        setTitle "dohaskell | requested edits"
        $(widgetFile "requested-edits-hub")
