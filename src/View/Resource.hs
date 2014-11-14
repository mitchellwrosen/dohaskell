module View.Resource
    ( editResourceForm
    , editResourceFormWidget
    , resourceCommentForestWidget
    , resourceCommentForm
    , resourceCommentTreeWidget
    , resourceForm
    , resourceInfoWidget
    ) where

import Import

import           Model.Resource
import           Yesod.Form.Types.Extra (commaSepTextField, mapField)

import           Data.List              (nub)
import qualified Data.Text              as T
import           Data.Tree              (Forest, Tree(..))
import           Yesod.Form.Bootstrap3  -- (renderBootstrap3)
import           Yesod.Markdown         (Markdown, markdownField)

-- Crappy type synonyms, trying not to clash with models. Unexported.
type AuthorNameText     = Text
type CollectionNameText = Text
type TagNameText        = Text
type Title              = Text
type YearPublished      = Int

-- | Form for editing a resource. Notably does not include the URL.
editResourceForm :: Maybe Title
                 -> Maybe [AuthorNameText]
                 -> Maybe (Maybe YearPublished)
                 -> Maybe ResourceType
                 -> Maybe [TagNameText]
                 -> Maybe [CollectionNameText]
                 -> Form (Title, [AuthorNameText], Maybe YearPublished, ResourceType, [TagNameText], [CollectionNameText])
editResourceForm title authors published typ tags colls = renderDivs $ (,,,,,)
    <$> resTitleForm       title
    <*> resAuthorsForm     authors
    <*> resPublishedForm   published
    <*> resTypeForm        typ
    <*> resTagsForm        tags
    <*> resCollectionsForm colls

editResourceFormWidget :: ResourceId
                       -> Maybe Title
                       -> Maybe [AuthorNameText]
                       -> Maybe (Maybe YearPublished)
                       -> Maybe ResourceType
                       -> Maybe [TagNameText]
                       -> Maybe [CollectionNameText]
                       -> Widget
editResourceFormWidget res_id title authors published typ tags colls = do
    (widget, enctype) <- handlerToWidget . generateFormPost $
        editResourceForm title authors published typ tags colls

    [whamlet|
      <form method=post action=@{EditResourceR res_id} enctype=#{enctype}>
        ^{widget}
        <input type="submit" value="Edit Resource">
    |]

-- | A single form to input a Resource.
resourceForm :: UserId -> Form ( Title
                               , Text                -- ^ URL.
                               , [AuthorNameText]
                               , Maybe YearPublished
                               , ResourceType
                               , [TagNameText]
                               , [CollectionNameText]
                               , UserId              -- ^ Poster.
                               , UTCTime             -- ^ Timestamp.
                               )
resourceForm uid = renderBootstrap3 BootstrapInlineForm $ (,,,,,,,,)
    <$> resTitleForm       Nothing
    <*> resUrlForm         Nothing
    <*> resAuthorsForm     Nothing
    <*> resPublishedForm   Nothing
    <*> resTypeForm        Nothing
    <*> resTagsForm        Nothing
    <*> resCollectionsForm Nothing
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
    <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

resTitleForm :: Maybe Title -> AForm Handler Title
resTitleForm = areq textField "Title"

resUrlForm :: Maybe Text -> AForm Handler Text
resUrlForm = areq urlField ("Url" {fsAttrs = [("placeholder", "http://")]})

resAuthorsForm :: Maybe [AuthorNameText] -> AForm Handler [AuthorNameText]
resAuthorsForm = fmap (fromMaybe []) . aopt field "Author(s) (optional, comma separated)" . fmap Just
  where
    field :: Field Handler [AuthorNameText]
    field = mapField nub commaSepTextField

resPublishedForm :: Maybe (Maybe YearPublished) -> AForm Handler (Maybe YearPublished)
resPublishedForm = aopt intField "Year (optional)"

resTypeForm :: Maybe ResourceType -> AForm Handler ResourceType
resTypeForm = areq field "Type"
  where
    field :: Field Handler ResourceType
    field = selectFieldList $ map (descResourceType &&& id) [minBound..maxBound]

resTagsForm :: Maybe [TagNameText] -> AForm Handler [TagNameText]
resTagsForm = areq field "Tags (comma separated)"
  where
    field :: Field Handler [Text]
    field = mapField nub commaSepTextField

resCollectionsForm :: Maybe [CollectionNameText] -> AForm Handler [CollectionNameText]
resCollectionsForm = fmap (fromMaybe []) . aopt field "Collection(s) (optional, comma separated)" . fmap Just
  where
    field :: Field Handler [Text]
    field = mapField nub commaSepTextField

-- | Display meta-information about the resource.
resourceInfoWidget :: Entity Resource -> Widget
resourceInfoWidget (Entity res_id res) = do
    (poster, authors, tags, colls) <- handlerToWidget . runDB $ (,,,)
        <$> get404 (resourceUserId res)
        <*> (map authorName     <$> fetchResourceAuthorsDB res_id)
        <*> (map tagName        <$> fetchResourceTagsDB res_id)
        <*> (map collectionName <$> fetchResourceCollectionsDB res_id)

    now <- liftIO getCurrentTime
    let posted = showDiffTime now (resourcePosted res)

    $(widgetFile "resource-info")

resourceCommentForestWidget :: UTCTime -> Map UserId User -> Forest (Entity Comment) -> Widget
resourceCommentForestWidget now users_map comment_forest =
    [whamlet|
      $forall comment_tree <- comment_forest
        ^{resourceCommentTreeWidget now users_map comment_tree}
    |]

resourceCommentTreeWidget :: UTCTime -> Map UserId User -> Tree (Entity Comment) -> Widget
resourceCommentTreeWidget now users_map (Node (Entity comment_id Comment{..}) children) = do
    let user = lookupErr "resourceCommentTreeWidget: commentUserId not found in users_map"
                         commentUserId
                         users_map

    [whamlet|
      <div .comment>
        <div .comment-head>
          <a .comment-user href=@{UserR commentUserId}>#{userDisplayName user}
          <span .comment-ts>#{showDiffTime now commentTimestamp} ago
        <div .comment-body>#{commentBody}
        <div .comment-actions>
          <a>reply

        ^{resourceCommentForestWidget now users_map children}
    |]

resourceCommentForm :: Maybe CommentId -> FieldSettings App -> Form (Maybe CommentId, Markdown)
resourceCommentForm mparent_id title = renderDivs $ (,)
  <$> pure mparent_id
  <*> areq markdownField title Nothing
