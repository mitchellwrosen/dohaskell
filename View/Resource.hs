module View.Resource
    ( resourceForm
    , resourceTagsForm
    , resourceTypeField
    , resourceInfoWidget
    , resourceInfoWidget'
    ) where

import Import

import           Data.Attoparsec.Text   (Parser, char, many1, notChar, sepBy1, skipSpace)
import qualified Data.Set               as S
import           Data.Text              (intercalate, pack)
import           Data.Time              (getCurrentTime)
import           Yesod.Form.Bootstrap3  -- (renderBootstrap3)

import           Model.Resource         (getResourceTags)
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
    <*> areq urlField "Url" (Just "http://")
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

-- Display meta-information about the resource (not including comments).
resourceInfoWidget :: ResourceId -> Widget
resourceInfoWidget resId = do
    (res,tags) <- handlerToWidget $ do
        res  <- runDB $ get404 resId
        tags <- runDB $ getResourceTags resId
        return (res,tags)
    resourceInfoWidget' res tags

-- Like resourceInfoWidget', but the caller provides the Resource and [Tag]. 
-- Useful for when the caller needs this information, but also wants to display 
-- this widget.
resourceInfoWidget' :: Resource -> [Tag] -> Widget
resourceInfoWidget' Resource{..} tags = do
    user <- handlerToWidget $ unsafeGetUserById resourceUserId
    $(widgetFile "resource-info")
