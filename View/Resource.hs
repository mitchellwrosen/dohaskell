module View.Resource
    ( resourceForm
    , resourceTagsForm
    , resourceTypeField
    , resourceWidget
    , resourceWidget'
    ) where

import Import

import Data.Attoparsec.Text   (Parser, char, many1, notChar, sepBy1, skipSpace)
import Data.Text              (intercalate, pack)
import Data.Time              (getCurrentTime)

import Model.Resource         (getResourceTags)
import Model.User             (unsafeGetUserById)
import Yesod.Form.Types.Extra (parsedTextField)

-- A single form to input a Resource and its associated Tags.
resourceForm :: UserId -> Form (Resource, [Tag])
resourceForm uid = renderDivs $ (,) <$> resourceEntityForm uid <*> resourceTagsForm Nothing

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
resourceTagsForm :: Maybe [Text] -> AForm Handler [Tag]
resourceTagsForm = (fmap.fmap) Tag . areq (parsedTextField parseTags showTags) "Tags (comma separated)"
  where
    parseTags :: Parser [Text]
    parseTags = parseTag `sepBy1` char ','

    parseTag :: Parser Text
    parseTag = pack <$> token (many1 $ notChar ',')

    token :: Parser a -> Parser a
    token p = skipSpace *> p <* skipSpace

    showTags :: [Text] -> Text
    showTags = intercalate ", "

-- Display meta-information about the resource (not including comments).
resourceWidget :: ResourceId -> Widget
resourceWidget resId = do
    (res,tags) <- handlerToWidget $ do
        res  <- runDB $ get404 resId
        tags <- runDB $ getResourceTags resId
        return (res,tags)
    resourceWidget' res tags

-- Like resourceWidget', but the caller provides the Resource and [Tag]. Useful
-- for when the caller needs this information, but also wants to display this
-- widget.
resourceWidget' :: Resource -> [Tag] -> Widget
resourceWidget' Resource{..} tags = do
    user <- handlerToWidget $ unsafeGetUserById resourceUserId
    [whamlet|
        <div>
           <div><a href=#{resourceUrl}>#{resourceTitle}</a> (#{resourceType})
           <div>Posted by <a href=@{UserR $ resourceUserId}>#{maybe "(none)" id $ userDisplayName user}</a> at #{show $ resourcePosted}
           <div >Tags:
              <ul>
                 $forall Tag text <- tags
                    <li>
                       <a href=@{TagR text}>#{text}
    |]
