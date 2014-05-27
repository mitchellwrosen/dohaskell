module Handler.Submit where

import Import

import Control.Applicative    ((<|>))
import Data.Attoparsec.Text   (Parser, char, digit, inClass, letter, many1, satisfy, sepBy1, skipSpace)
import Data.Text              (intercalate, pack)
import Data.Time              (getCurrentTime)
import Yesod.Auth
import Yesod.Form.Types.Extra

getSubmitR :: Handler Html
getSubmitR = do
    uid <- requireAuthId
    (widget, enctype) <- generateFormPost (resourceForm uid)
    defaultLayout $ do
        setTitle "Submit a resource"
        $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
    ((result, widget), enctype) <- runFormPost . resourceForm =<< requireAuthId
    case result of
        FormSuccess (res,tags) -> do
            runDB (insertBy res) >>= \case 
                Left (Entity resId _) -> do
                    setDuplicateUrlMessage resId
                    defaultLayout $ do
                        setTitle "Submit a resource"
                        $(widgetFile "submit")
                Right resId -> do
                    -- It's okay to ignore returned ids, attempting to insert
                    -- duplicate ResourceTags is okay.
                    runDB $ mapM_ (insertResourceTagAndTag resId) tags
                    setMessage "Thanks for your submission!"
                    redirect HomeR
        _ -> do
            setMessage "Invalid resource submission! Please try again."
            redirect SubmitR
  where
    insertResourceTagAndTag :: ResourceId -> Tag -> SqlPersistT Handler (Maybe ResourceTagId)
    insertResourceTagAndTag resId tag = do
        insertBy tag >>= \case
            Left (Entity tagId _) -> insertUnique $ ResourceTag resId tagId
            Right tagId           -> insertUnique $ ResourceTag resId tagId

    setDuplicateUrlMessage :: ResourceId -> Handler ()
    setDuplicateUrlMessage resId = do
        giveUrlRenderer [hamlet|
                URL already exists.
                <a href=@{ResourceR resId}>Take me there.
            |] >>= setMessage

resourceForm :: UserId -> Form (Resource, [Tag])
resourceForm uid = renderDivs $ (,) <$> resourceEntityForm uid <*> tagsForm

resourceEntityForm :: UserId -> AForm Handler Resource
resourceEntityForm uid = Resource
    <$> areq textField "Title" Nothing
    <*> areq urlField "Url" (Just "http://")
    <*> areq (selectFieldList fields) "Type" Nothing
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
  where
    fields :: [(Text, Text)]
    fields = 
        [ ("Blog post",                         "blog")
        , ("Forum post (e.g. Reddit comment)",  "forum")
        , ("Lecture slides/notes",              "lecture")
        , ("Research paper",                    "research")
        , ("Q&A website (e.g. Stack Overflow)", "q&a")
        , ("Video lecture/presentation",        "video")
        ]

tagsForm :: AForm Handler [Tag]
tagsForm = fmap Tag <$> areq (parsedTextField parseTags showTags) "Tags (comma separated)" Nothing
  where
    showTags :: [Text] -> Text
    showTags = intercalate ","

parseTags :: Parser [Text]
parseTags = parseTag `sepBy1` char ','

parseTag :: Parser Text
parseTag = pack <$> token (many1 parseTagCharacter)

parseTagCharacter :: Parser Char
parseTagCharacter = satisfy (inClass "a-zA-Z0-9-")

alphaNum :: Parser Char
alphaNum = letter <|> digit

token :: Parser a -> Parser a
token p = skipSpace *> p <* skipSpace
