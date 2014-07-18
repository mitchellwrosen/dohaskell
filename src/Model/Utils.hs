module Model.Utils where

import Import

import           Data.List            (sortBy)
import qualified Data.Text            as T
import           Database.Persist.Sql (SqlBackend)

getAllEntities :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)  => (Entity val -> Entity val -> Ordering) -> YesodDB App [Entity val]
getAllEntities f = sortBy f <$> selectList [] []

alphabeticIgnoreCase :: (val -> Text) -> Entity val -> Entity val -> Ordering
alphabeticIgnoreCase textFunc (Entity _ val1) (Entity _ val2) =
    T.toLower (textFunc val1) `compare` T.toLower (textFunc val2)
