module View.Resource
    ( resourceForm
    , resourceTagsForm
    , resourceTypeField
    , resourceInfoWidget
    , resourceListWidget
    , resourceListItemWidget
    ) where

import Import

import           Data.Attoparsec.Text   (Parser, char, many1, notChar, sepBy1, skipSpace)
import           Data.Foldable          (Foldable)
import           Data.Maybe             (isJust)
import qualified Data.Set               as S
import           Data.Text              (intercalate, pack)
import           Data.Time              (getCurrentTime)
import           Yesod.Form.Bootstrap3  -- (renderBootstrap3)

import           Model.Resource         (getFavoriteResources, getGrokkedResources, getResourceTags)
import           Model.User             (unsafeGetUserById)
import           Yesod.Form.Types.Extra (parsedTextField)

-- A single form to input a Resource and its associated tags.
resourceForm :: UserId -> Form (Resource, Set Text)
resourceForm uid = renderBootstrap3 BootstrapInlineForm $ (,)
    <$> resourceEntityForm uid
    <*> resourceTagsForm Nothing
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

-- A form to input a Resource.
resourceEntityForm :: UserId -> AForm Handler Resource
resourceEntityForm uid = Resource
    <$> areq textField "Title" Nothing
    <*> areq urlField ("Url" {fsAttrs = [("placeholder", "http://")]}) Nothing
    <*> aopt textField "Primary Author (optional)" Nothing
    <*> aopt intField "Year (optional)" Nothing
    <*> areq resourceTypeField "Type" Nothing
    <*> pure uid
    <*> lift (liftIO getCurrentTime)

resourceTypeField :: Field Handler ResourceType
resourceTypeField = selectFieldList $ map (descResourceType &&& id) [minBound..maxBound]

-- A form to input a comma-separated list of tags.
resourceTagsForm :: Maybe (Set Text) -> AForm Handler (Set Text)
resourceTagsForm = areq (parsedTextField parseTags showTags) "Tags (comma separated)"
  where
    parseTags :: Parser (Set Text)
    parseTags = S.fromList <$> parseTag `sepBy1` char ','

    parseTag :: Parser Text
    parseTag = pack <$> token (many1 $ notChar ',')

    token :: Parser a -> Parser a
    token p = skipSpace *> p <* skipSpace

    showTags :: Set Text -> Text
    showTags = intercalate ", " . S.toAscList

-- Display meta-information about the resource.
resourceInfoWidget :: Entity Resource -> Widget
resourceInfoWidget (Entity resId res) = do
    tags <- handlerToWidget . runDB $ getResourceTags resId
    user <- handlerToWidget . unsafeGetUserById $ resourceUserId res
    $(widgetFile "resource-info")

resourceListWidget :: Foldable f => f (Entity Resource) -> Text -> Widget
resourceListWidget resources title = do
    favsAndGrokked <- handlerToWidget getFavoritesAndGrokkedResources
    setTitle $ toHtml title
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    $(widgetFile "resource-list")

resourceListItemWidget :: Entity Resource -> Maybe (Set ResourceId, Set ResourceId) -> Widget
resourceListItemWidget (Entity resId Resource{..}) favsAndGrokked = do
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

          <div .res-published-type-author>
            $maybe published <- resourcePublished
              <span .res-published>#{show published}
            <span .res-type>#{shortDescResourceType resourceType}
              $maybe author <- resourceAuthor
                <span .res-author-by> by
                <span .res-author> #{author}


    |]

-- TODO: Find a better module for this. Model/Resource...?
getFavoritesAndGrokkedResources :: Handler (Maybe (Set ResourceId, Set ResourceId))
getFavoritesAndGrokkedResources = maybeAuthId >>= \case
    Nothing  -> return Nothing
    Just uid -> runDB $ Just <$> ((,) <$> getFavoriteResources uid <*> getGrokkedResources uid)
