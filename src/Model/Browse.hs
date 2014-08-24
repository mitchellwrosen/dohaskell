module Model.Browse
    ( BrowseByLink(..)
    , SortBy(..)
    ) where

import Import

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
    | SortByYearUp    -- earliest year at top
    | SortByYearDown  -- latest year at top
    deriving Eq

instance Show SortBy where
    show SortByAZ        = "a-z"
    show SortByCountUp   = "count-up"
    show SortByCountDown = "count-down"
    show SortByYearUp    = "year-up"
    show SortByYearDown  = "year-down"
