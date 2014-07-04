module Model where

import Data.Text              (Text)
import Data.Time              (UTCTime)
import Data.Typeable          (Typeable)
import Database.Persist.Quasi
import Prelude                (Bool, Eq, Int, Ord)
import Yesod

import Model.ResourceType     (ResourceType)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [ mkPersist sqlOnlySettings
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlOnlySettings
      ]
    $(persistFileWith lowerCaseSettings "config/models")
