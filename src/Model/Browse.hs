module Model.Browse
    ( BrowseByLink(..)
    , SortBy(..)
    , getTypeCounts -- TODO: move this
    ) where

import Import

import Model.Resource

import qualified Data.Map           as M
import           Database.Esqueleto

-- | Get a map of ResourceType to the number of Resources with that type.
getTypeCounts :: YesodDB App (Map ResourceType Int)
getTypeCounts = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value ResourceType, Value Int)]
    sel = select $
          from $ \r -> do
          groupBy (r^.ResourceType)
          return (r^.ResourceType, countRows)

data BrowseByLink
    = BrowseByAuthorLink
    | BrowseByResourceLink
    | BrowseByTagLink
    | BrowseByTypeLink
    deriving Eq

data SortBy
    = SortByAZ
    | SortByCountUp   -- lowest count at top
    | SortByCountDown -- highest count at top
    | SortByEarliest  -- earliest year at top
    | SortByLatest    -- latest year at top
    deriving Eq
