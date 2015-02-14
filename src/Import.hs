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
import           Data.Time                    as Import (UTCTime, diffUTCTime, getCurrentTime)

import           Foundation                   as Import
import           Model                        as Import
import           Settings                     as Import
import           Settings.Development         as Import
import           Settings.StaticFiles         as Import

import Database.Esqueleto
import Yesod.Fay

import qualified Data.Map as M

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

fayFile :: FayFile
fayFile module_name
    | development = fayFileReload settings
    | otherwise   = fayFileProd   settings
  where
    settings = YesodFaySettings
        { yfsModuleName      = module_name
        , yfsSeparateRuntime = Nothing
        , yfsPostProcess     = return
        , yfsExternal        = Nothing
        , yfsRequireJQuery   = True
        , yfsPackages        = ["fay-base", "fay-dom", "fay-jquery", "fay-text"]
        , yfsTypecheckDevel  = False
        }

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

-- | Lookup a key in a map; if it doesn't exist, error.
lookupErr :: Ord k => String -> k -> Map k a -> a
lookupErr = M.findWithDefault . error

showDiffTime :: UTCTime -> UTCTime -> String
showDiffTime x y =
  let secs_ago = round (diffUTCTime x y)
  in if | secs_ago < secsPerHour  -> go secs_ago secsPerMinute "m"
        | secs_ago < secsPerDay   -> go secs_ago secsPerHour   "h"
        | secs_ago < secsPerWeek  -> go secs_ago secsPerDay    "d"
        | secs_ago < secsPerMonth -> go secs_ago secsPerWeek   "wk"
        | secs_ago < secsPerYear  -> go secs_ago secsPerMonth  "mo"
        | otherwise               -> go secs_ago secsPerYear   "yr"
  where
    go secs_ago divisor suffix = show (secs_ago `div` divisor) ++ suffix

    secsPerMinute, secsPerHour, secsPerDay, secsPerWeek, secsPerMonth, secsPerYear :: Integer
    secsPerMinute = 60
    secsPerHour   = 3600     -- 60*60
    secsPerDay    = 86400    -- 60*60*24
    secsPerWeek   = 604800   -- 60*60*24*7
    secsPerMonth  = 2592000  -- 60*60*24*30
    secsPerYear   = 31536000 -- 60*60*24*365

-- | Create a map from id -> value, from a list of entities.
entitiesMap :: Ord (Key a) => [Entity a] -> Map (Key a) a
entitiesMap = foldr (\(Entity k v) -> M.insert k v) mempty
