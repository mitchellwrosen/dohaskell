module Model where

import Model.Feed.Internal
import Model.Resource.Internal

import Data.ByteString        (ByteString)
import Data.Text              (Text)
import Data.Time              (UTCTime)
import Data.Typeable          (Typeable)
import Database.Persist.Quasi
import Prelude                (Bool, Eq, Int, Ord)
import Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [ mkPersist sqlOnlySettings
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlOnlySettings
      ]
    $(persistFileWith lowerCaseSettings "config/models")
