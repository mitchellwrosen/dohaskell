{-# LANGUAGE OverloadedStrings #-}

module Model.ResourceType where

import Prelude
import Control.Monad
import Data.Text (Text)
import Database.Persist.Sql

data ResourceType 
    = BlogPostResource
    | LectureResource
    | TutorialResource
    | VideoResource
    deriving (Eq, Read, Show)

instance PersistField ResourceType where
    toPersistValue = toPersistValue . show
    fromPersistValue val =
        case fromPersistValue val :: Either Text String of
            Left e -> Left e
            Right s -> case reads s of
                [(r,"")] -> Right r
                _        -> Left "could not read ResourceType"

instance PersistFieldSql ResourceType where
    sqlType = sqlType . liftM show
