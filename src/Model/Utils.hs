module Model.Utils where

import Import

import Data.List              (sortBy)
import Database.Persist.Sql   (SqlBackend)

getAllEntities :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)  => (Entity val -> Entity val -> Ordering) -> YesodDB App [Entity val]
getAllEntities f = sortBy f <$> selectList [] []
