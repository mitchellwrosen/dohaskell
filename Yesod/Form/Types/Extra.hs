module Yesod.Form.Types.Extra 
    ( parsedTextField
    ) where

import Prelude

import Data.Attoparsec.Text  (Parser, parseOnly)
import Data.Monoid           ((<>))
import Data.Text             (Text, pack)
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core            (HandlerSite)
import Yesod.Core.Widget     (whamlet)
import Yesod.Form.Types      (Enctype(..), Field(..), FormMessage(..))
import Yesod.Form.Functions  (parseHelper)

parsedTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Parser a -> (a -> Text) -> Field m a
parsedTextField parser shower = Field
    { fieldParse = parseHelper $ \s ->
        case parseOnly parser s of
            Left err -> Left $ MsgInvalidEntry $ "Parse error: " <> (pack err)
            Right xs -> Right xs
    , fieldView = \theId name attrs val isReq ->
            [whamlet|
                $newline never
                <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id shower val}">
            |]
    , fieldEnctype = UrlEncoded
    }
