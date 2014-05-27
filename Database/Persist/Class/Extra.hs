module Database.Persist.Class.Extra where

import Control.Monad (liftM)
import Database.Persist.Class
import Database.Persist.Types
import Prelude

-- Like insertBy, but only return the Key if the Entity exists, not the whole Entity
insertBy' :: (PersistEntity val, PersistUnique m, PersistEntityBackend val ~ PersistMonadBackend m)
          => val
          -> m (Key val)
insertBy' = liftM (either entityKey id) . insertBy
