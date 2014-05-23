module Yesod.Form.Types.Extra where

import Prelude
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core            (HandlerSite)
import Yesod.Core.Widget     (whamlet)
import Yesod.Form.Types      (Enctype(..), Field(..), FormMessage(..))
import Yesod.Form.Functions  (parseHelper)
import Data.List.Split       (splitOn)
import Data.Text             (Text, intercalate, pack, unpack)

delimitedTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m [Text]
delimitedTextField delim = Field
    { fieldParse = parseHelper $ \s ->
        case map pack . splitOn (unpack delim) . unpack $ s of
            [] -> Left $ MsgInvalidEntry "at least one field required"
            xs -> Right xs
    , fieldView = \theId name attrs val isReq ->
            [whamlet|
            $newline never
            <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id cat val}">
            |]
    , fieldEnctype = UrlEncoded
    }
  where cat = intercalate delim

commaSeparatedTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m [Text]
commaSeparatedTextField = delimitedTextField ","
