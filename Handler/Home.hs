module Handler.Home where

import Import

import           Data.Maybe     (fromJust)
import qualified Data.Map       as M

import           Model.Tag      (getAllTags, getTagCounts)
import           Model.Resource (getGrokkedCounts)

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuthId

    -- tags          :: [Entity Tag]
    -- tagCounts     :: Map TagId Int
    -- grokkedCounts :: Maybe (Map TagId Int)
    (tags, tagCounts, mgrokkedCounts) <- runDB $ (,,)
        <$> getAllTags
        <*> getTagCounts
        <*> maybe (return Nothing) (fmap Just . getGrokkedCounts) muid

    defaultLayout $ do
        setTitle "dohaskell: tagged Haskell learning resources"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        $(widgetFile "homepage")
