module Import
    ( module Import
    ) where

import           Prelude                      as Import hiding (head, init, last, readFile, tail, writeFile, mapM_)
import           Yesod                        as Import hiding (Route(..), Value, (==.), (!=.), (*=.), (+=.)
                                                               , (-=.), (/=.), (<.), (<=.), (>.), (>=.)
                                                               , (=.), (||.), count, delete, selectSource, update
                                                               )
import           Yesod.Auth                   as Import

import           Control.Applicative          as Import ((<$>), (<*>), (*>), (<*), liftA2, liftA3, pure)
import           Control.Arrow                as Import ((&&&), (***))
import           Control.Monad                as Import (liftM, join, unless, void, when)
import           Data.Foldable                as Import (forM_, mapM_)
import           Data.List                    as Import (foldl')
import           Data.Map                     as Import (Map)
import           Data.Maybe                   as Import (fromMaybe)
import           Data.Set                     as Import (Set)
import           Data.Text                    as Import (Text)
import           Data.Time                    as Import (UTCTime, getCurrentTime)

import           Foundation                   as Import
import           Model                        as Import
import           Settings                     as Import
import           Settings.Development         as Import
import           Settings.StaticFiles         as Import

import Database.Esqueleto

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

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
