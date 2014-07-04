module View.Resource
    ( editResourceForm
    , resourceForm
    , resourceInfoWidget
    , resourceListWidget
    , resourceListItemWidget
    ) where

import Import

import           Model.Resource         (getAuthors, getAuthorsIn, getTags)
import           Model.User             (getFavoriteResourcesIn, getGrokkedResourcesIn)
import           Yesod.Form.Types.Extra (commaSepTextField, fmapField, mapField)

import           Data.List              (nub)
import           Data.Maybe             (isJust)
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
                 -> Maybe (Set Text)   -- default tags
                 -> Form (Text, [Text], Maybe Int, ResourceType, Set Text)
editResourceForm title authors published typ tags = renderDivs $ (,,,,)
    <$> resTitleForm     title
    <*> resAuthorsForm   authors
    <*> resPublishedForm published
    <*> resTypeForm      typ
    <*> resTagsForm      tags

-- | A single form to input a Resource, along with its Tags and Authors
-- (which are separate types, hence the giant tuple).
resourceForm :: UserId -> Form ( Text         -- title
                               , Text         -- url
                               , [Text]       -- authors
                               , Maybe Int    -- year
                               , ResourceType -- type
                               , Set Text     -- tags
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
resAuthorsForm = areq authorsField "Author(s) (optional, comma separated)"
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

resTagsForm :: Maybe (Set Text) -> AForm Handler (Set Text)
resTagsForm = areq tagsField "Tags (comma separated)"
  where
    tagsField :: Field Handler (Set Text)
    tagsField = fmapField S.fromList S.toAscList commaSepTextField

-- | Display meta-information about the resource.
resourceInfoWidget :: Entity Resource -> Widget
resourceInfoWidget (Entity res_id res) = do
    (tags, authors, poster) <- handlerToWidget . runDB $ (,,)
        <$> getTags res_id
        <*> getAuthors res_id
        <*> getJust (resourceUserId res)
    $(widgetFile "resource-info")

resourceListWidget :: [(Entity Resource)] -> Text -> Widget
resourceListWidget resources title = do
    let resource_ids = map entityKey resources

    authorsMap <- handlerToWidget . runDB $ getAuthorsIn resource_ids

    -- favsAndGrokked :: Maybe (Set ResourceId, Set ResourceId)
    favsAndGrokked <- handlerToWidget $
        maybeAuthId >>= \case
            Nothing  -> return Nothing
            Just uid -> fmap Just . runDB $ (,)
                <$> getFavoriteResourcesIn resource_ids uid
                <*> getGrokkedResourcesIn  resource_ids uid


    setTitle $ toHtml title
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    $(widgetFile "resource-list")

resourceListItemWidget :: Entity Resource
                       -> Map ResourceId [Author]
                       -> Maybe (Set ResourceId, Set ResourceId)
                       -> Widget
resourceListItemWidget (Entity resId Resource{..}) authorsMap favsAndGrokked = do
    let is_logged_in = isJust favsAndGrokked
        is_fav       = maybe False (S.member resId . fst) favsAndGrokked
        is_grokked   = maybe False (S.member resId . snd) favsAndGrokked

    [whamlet|
      <div .res-li>

        $if is_logged_in
          <div .res-fav :is_fav:.fav ##{toPathPiece resId} title="Favorite">
          <div .res-grok :is_grokked:.grok ##{toPathPiece resId} title="Grokked">

        <a .res-info href=@{ResourceR resId}>

        <a .res-link href=#{resourceUrl}>
          <div .res-title>#{resourceTitle}

          <div .res-published-type-authors>
            $maybe published <- resourcePublished
              <span .res-published>#{show published}
            <span .res-type>#{shortDescResourceType resourceType}
            $maybe authors <- map authorName <$> M.lookup resId authorsMap
                <span .res-author-by> by
                <span .res-authors> #{T.intercalate ", " $ authors}
    |]
