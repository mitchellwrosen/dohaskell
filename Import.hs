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
import           Control.Arrow                as Import ((&&&))
import           Control.Monad                as Import (join, unless, void, when)
import           Data.Foldable                as Import (forM_, mapM_)
import           Data.Map                     as Import (Map)
import           Data.Set                     as Import (Set)
import           Data.Text                    as Import (Text)

import           Foundation                   as Import
import           Model                        as Import
import           Model.ResourceType           as Import
import           Settings                     as Import
import           Settings.Development         as Import
import           Settings.StaticFiles         as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
