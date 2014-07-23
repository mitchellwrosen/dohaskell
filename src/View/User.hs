module View.User where

import Import

import           Data.Char          (isAlphaNum)
import qualified Data.Text          as T

displayNameForm :: Maybe Text -> Form Text
displayNameForm = renderDivs . areq (validateNameField textField) ""
  where
    validateNameField :: Field Handler Text -> Field Handler Text
    validateNameField = checkBool validName ("Only alphanumeric characters allowed."::Text)

    validName :: Text -> Bool
    validName = allCharsSatisfy isAlphaNum

    allCharsSatisfy :: (Char -> Bool) -> Text -> Bool
    allCharsSatisfy f = T.foldr (\c b -> f c && b) True
