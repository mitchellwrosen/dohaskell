{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Form.Types.Extra
    ( commaSepTextField
    , fmapField
    , mapField
    , parsedTextField
    ) where

import Import

import Data.Attoparsec.Text (Parser, char, many1, notChar, parseOnly, sepBy1, skipSpace)

import qualified Data.Text as T

commaSepTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m [Text]
commaSepTextField = parsedTextField parseTexts showTexts
  where
    parseTexts :: Parser [Text]
    parseTexts = parseText `sepBy1` char ','

    parseText :: Parser Text
    parseText = T.pack <$> token (many1 $ notChar ',')

    token :: Parser a -> Parser a
    token p = skipSpace *> p <* skipSpace

    showTexts :: [Text] -> Text
    showTexts = T.intercalate ", "

fmapField :: forall a b m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => (a -> b) -> (b -> a) -> Field m a -> Field m b
fmapField f = checkMMap (return . Right . f :: a -> m (Either FormMessage b))

mapField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => (a -> a) -> Field m a -> Field m a
mapField f = fmapField f id

parsedTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Parser a -> (a -> Text) -> Field m a
parsedTextField parser shower = Field
    { fieldParse = parseHelper $ \s ->
        case parseOnly parser s of
            Left err -> Left $ MsgInvalidEntry $ "Parse error: " <> (T.pack err)
            Right xs -> Right xs
    , fieldView = \theId name attrs val isReq ->
            [whamlet|
                $newline never
                <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id shower val}">
            |]
    , fieldEnctype = UrlEncoded
    }
