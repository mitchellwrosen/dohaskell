module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import Database.Esqueleto

class FromValue a where
    type UnValue a
    fromValue :: a -> UnValue a

instance FromValue (Value a) where
    type UnValue (Value a) = a
    fromValue = unValue

instance (FromValue a, FromValue b) => FromValue (a, b) where
    type UnValue (a, b) = (UnValue a, UnValue b)
    fromValue (a, b) = (fromValue a, fromValue b)

instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c) where
    type UnValue (a, b, c) = (UnValue a, UnValue b, UnValue c)
    fromValue (a, b, c) = (fromValue a, fromValue b, fromValue c)
