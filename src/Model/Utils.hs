module Model.Utils where

import Import

import qualified Data.Map             as M
import qualified Data.Text            as T

orderAlphabeticIgnoreCase :: (a -> Text) -> a -> a -> Ordering
orderAlphabeticIgnoreCase f x y = T.toLower (f x) `compare` T.toLower (f y)

-- | Order by lowest-count-first. Secondary key: alphabetical.
orderCountUp :: Ord b => (a -> Text) -> (a -> b) -> Map b Int -> a -> a -> Ordering
orderCountUp text_func key_func count_map x y = compareCounts <> orderAlphabeticIgnoreCase text_func x y
  where
    compareCounts = case (M.lookup (key_func x) count_map, M.lookup (key_func y) count_map) of
        (Just n, Just m)  -> compare n m
        (Just _, Nothing) -> GT -- Nothing means 0
        (Nothing, Just _) -> LT
        _                 -> EQ

-- | Order by highest-count-first. Secondary key: alphabetical.
orderCountDown :: Ord b => (a -> Text) -> (a -> b) -> Map b Int -> a -> a -> Ordering
orderCountDown text_func key_func count_map x y = compareCounts <> orderAlphabeticIgnoreCase text_func x y
  where
    compareCounts = case (M.lookup (key_func x) count_map, M.lookup (key_func y) count_map) of
        (Just n, Just m)  -> compare m n
        (Just _, Nothing) -> LT -- Nothing means 0
        (Nothing, Just _) -> GT
        _                 -> EQ

-- | Order by earliest-first-year-first (e.g. "1999-2014" comes before "2000-2001".)
-- Secondary key: alphabetical.
orderYearUp :: Ord b => (a -> Text) -> (a -> b) -> Map b (Int, Int) -> a -> a -> Ordering
orderYearUp text_func key_func year_ranges_map x y = compareEarliest <> orderAlphabeticIgnoreCase text_func x y
  where
    compareEarliest = case (M.lookup (key_func x) year_ranges_map, M.lookup (key_func y) year_ranges_map) of
        (Just (n, _), Just (m, _)) -> compare n m
        (Just _,      Nothing)     -> LT -- Nothing means no year, so put it at the bottom
        (Nothing,     Just _)      -> GT
        _                          -> EQ

-- | Order by latest-last-year-first (e.g. "1999-2014" comes before "2000-2001".)
-- Secondary key: alphabetical.
orderYearDown :: Ord b => (a -> Text) -> (a -> b) -> Map b (Int, Int) -> a -> a -> Ordering
orderYearDown text_func key_func year_ranges_map x y = compareLatest <> orderAlphabeticIgnoreCase text_func x y
  where
    compareLatest = case (M.lookup (key_func x) year_ranges_map, M.lookup (key_func y) year_ranges_map) of
        (Just (_, n), Just (_, m)) -> compare m n
        (Just _,      Nothing)     -> LT -- Nothing means no year, so put it at the bottom
        (Nothing,     Just _)      -> GT
        _                          -> EQ
