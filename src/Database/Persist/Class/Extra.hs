module Database.Persist.Class.Extra where

import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class
import Database.Persist.Types
import Prelude

-- Like insertBy, but only return the Key if the Entity exists, not the whole Entity
insertBy' :: (MonadIO m, PersistEntity val, PersistUnique backend, PersistEntityBackend val ~ backend)
          => val
          -> ReaderT backend m (Key val)
insertBy' = fmap (either entityKey id) . insertBy
