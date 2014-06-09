module Handler.ReqEditsHub where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import Handler.Utils      (denyPermissionIfDifferentUser)
import Model.ResourceEdit
import View.Resource      (resourceInfoWidget)

getReqEditsHubR :: UserId -> Handler Html
getReqEditsHubR uid = do
    denyPermissionIfDifferentUser uid

    (editTitles, editAuthors, editTypes, editAddTags, editRemoveTags) <-
        runDB $ (,,,,)
            <$> getEditTitles     uid
            <*> getEditAuthors    uid
            <*> getEditTypes      uid
            <*> getEditAddTags    uid
            <*> getEditRemoveTags uid

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
