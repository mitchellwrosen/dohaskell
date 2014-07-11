module View.Resource
    ( editResourceForm
    , editResourceFormWidget
    , resourceForm
    , resourceInfoWidget
    , resourceListWidget
    ) where

import Import

import           Handler.Utils          (prettyAgo)
import           Model.Resource
import           Model.User             (getFavoriteResourcesIn, getGrokkedResourcesIn)
import           Yesod.Form.Types.Extra (commaSepTextField, mapField)

import           Data.List              (nub)
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Time              (UTCTime, getCurrentTime)
import           Yesod.Form.Bootstrap3  -- (renderBootstrap3)

-- | Form for editing a resource. Notably does not include the URL.
editResourceForm :: Maybe Text         -- default title
                 -> Maybe [Text]       -- default authors
                 -> Maybe (Maybe Int)  -- default published
                 -> Maybe ResourceType -- default type
                 -> Maybe [Text]       -- default tags
                 -> Form (Text, [Text], Maybe Int, ResourceType, [Text])
editResourceForm title authors published typ tags = renderDivs $ (,,,,)
    <$> resTitleForm     title
    <*> resAuthorsForm   authors
    <*> resPublishedForm published
    <*> resTypeForm      typ
    <*> resTagsForm      tags

editResourceFormWidget :: ResourceId
                       -> Maybe Text         -- default title
                       -> Maybe [Text]       -- default authors
                       -> Maybe (Maybe Int)  -- default published
                       -> Maybe ResourceType -- default type
                       -> Maybe [Text]       -- default tags
                       -> Widget
editResourceFormWidget res_id title authors published typ tags = do
    (widget, enctype) <- handlerToWidget . generateFormPost $
        editResourceForm title authors published typ tags

    [whamlet|
      <form method=post action=@{EditResourceR res_id} enctype=#{enctype}>
        ^{widget}
        <input type="submit" value="Edit Resource">
    |]

-- | A single form to input a Resource, along with its Tags and Authors
-- (which are separate types, hence the giant tuple).
resourceForm :: UserId -> Form ( Text         -- title
                               , Text         -- url
                               , [Text]       -- authors
                               , Maybe Int    -- year
                               , ResourceType -- type
                               , [Text]       -- tags
                               , UserId       -- poster id
                               , UTCTime      -- timestamp
                               )
resourceForm uid = renderBootstrap3 BootstrapInlineForm $ (,,,,,,,)
    <$> resTitleForm     Nothing
    <*> resUrlForm       Nothing
    <*> resAuthorsForm   Nothing
    <*> resPublishedForm Nothing
    <*> resTypeForm      Nothing
    <*> resTagsForm      Nothing
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

resTitleForm :: Maybe Text -> AForm Handler Text
resTitleForm = areq textField "Title"

resUrlForm :: Maybe Text -> AForm Handler Text
resUrlForm = areq urlField ("Url" {fsAttrs = [("placeholder", "http://")]})

resAuthorsForm :: Maybe [Text] -> AForm Handler [Text]
resAuthorsForm = fmap (fromMaybe []) . aopt authorsField "Author(s) (optional, comma separated)" . fmap Just
  where
    authorsField :: Field Handler [Text]
    authorsField = mapField nub commaSepTextField

resPublishedForm :: Maybe (Maybe Int) -> AForm Handler (Maybe Int)
resPublishedForm = aopt intField "Year (optional)"

resTypeForm :: Maybe ResourceType -> AForm Handler ResourceType
resTypeForm = areq typeField "Type"
  where
    typeField :: Field Handler ResourceType
    typeField = selectFieldList $ map (descResourceType &&& id) [minBound..maxBound]

resTagsForm :: Maybe [Text] -> AForm Handler [Text]
resTagsForm = areq tagsField "Tags (comma separated)"
  where
    tagsField :: Field Handler [Text]
    tagsField = mapField nub commaSepTextField

-- | Display meta-information about the resource.
resourceInfoWidget :: Entity Resource -> Widget
resourceInfoWidget (Entity res_id res) = do
    (tags, authors, poster) <- handlerToWidget . runDB $ (,,)
        <$> getTags res_id
        <*> getAuthorNames res_id
        <*> getJust (resourceUserId res)
    posted <- prettyAgo (resourcePosted res)
    $(widgetFile "resource-info")

resourceListWidget :: [(Entity Resource)] -> Text -> Widget
resourceListWidget resources title = do
    let resource_ids = map entityKey resources

    authorsMap <- handlerToWidget . runDB $ getAuthorsIn resource_ids

    (is_logged_in, favs, grokked) <- handlerToWidget $
        maybeAuthId >>= \case
            Nothing  -> return (False, mempty, mempty)
            Just uid -> runDB $ (,,)
                <$> pure True
                <*> getFavoriteResourcesIn resource_ids uid
                <*> getGrokkedResourcesIn  resource_ids uid


    setTitle $ toHtml title
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    $(widgetFile "resource-list")
