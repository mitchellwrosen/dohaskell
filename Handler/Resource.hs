{-# LANGUAGE TupleSections #-}

module Handler.Resource where

import Import

import qualified Data.Set          as S

import           Model.Resource    (getResourceTags)
import           Model.User        (thisUserHasAuthorityOver)
import           View.Resource     (resourceInfoWidget)
import           View.EditResource (editResourceForm)

getResourceR :: ResourceId -> Handler Html
getResourceR resId = do
    res  <- runDB $ get404 resId
    tags <- runDB $ getResourceTags resId
    (widget, enctype) <- generateFormPost $
        editResourceForm (Just $ resourceTitle res)
                         (Just $ resourceAuthor res)
                         (Just $ resourceType res)
                         (Just . S.fromList $ map tagText tags)
    canDelete <- thisUserHasAuthorityOver (resourceUserId res)
    defaultLayout $ do
        setTitle . toHtml $ "dohaskell | " <> resourceTitle res
        $(widgetFile "resource")
