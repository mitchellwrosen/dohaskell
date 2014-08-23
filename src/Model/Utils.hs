module Model.Utils where

import Import

import qualified Data.Map             as M
import qualified Data.Text            as T

orderAlphabeticIgnoreCase :: (val -> Text) -> Entity val -> Entity val -> Ordering
orderAlphabeticIgnoreCase text_func (Entity _ val1) (Entity _ val2) =
    T.toLower (text_func val1) `compare` T.toLower (text_func val2)

-- | Order by lowest-count-first. Secondary key: alphabetical.
orderCountUp :: (val -> Text) -> Map (Key val) Int -> Entity val -> Entity val -> Ordering
orderCountUp text_func count_map val1 val2 = compareCounts <> orderAlphabeticIgnoreCase text_func val1 val2
  where
    compareCounts = case (M.lookup (entityKey val1) count_map, M.lookup (entityKey val2) count_map) of
        (Just n, Just m)  -> compare n m
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        _                 -> EQ

-- | Order by highest-count-first. Secondary key: alphabetical.
orderCountDown :: (val -> Text) -> Map (Key val) Int -> Entity val -> Entity val -> Ordering
orderCountDown text_func count_map val1 val2 = compareCounts <> orderAlphabeticIgnoreCase text_func val1 val2
  where
    compareCounts = case (M.lookup (entityKey val1) count_map, M.lookup (entityKey val2) count_map) of
        (Just n, Just m)  -> compare m n
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        _                 -> EQ

-- | Order by earliest-first-year-first (e.g. "1999-2014" comes before "2000-2001".)
-- Secondary key: alphabetical.
orderEarliest :: (val -> Text) -> Map (Key val) (Int, Int) -> Entity val -> Entity val -> Ordering
orderEarliest text_func year_ranges_map val1 val2 = compareEarliest <> orderAlphabeticIgnoreCase text_func val1 val2
  where
    compareEarliest = case (M.lookup (entityKey val1) year_ranges_map, M.lookup (entityKey val2) year_ranges_map) of
        (Just (n, _), Just (m, _)) -> compare n m
        (Just _,      Nothing)     -> LT
        (Nothing,     Just _)      -> GT
        _                          -> EQ

-- | Order by latest-last-year-first (e.g. "1999-2014" comes before "2000-2001".)
-- Secondary key: alphabetical.
orderLatest :: (val -> Text) -> Map (Key val) (Int, Int) -> Entity val -> Entity val -> Ordering
orderLatest text_func year_ranges_map val1 val2 = compareLatest <> orderAlphabeticIgnoreCase text_func val1 val2
  where
    compareLatest = case (M.lookup (entityKey val1) year_ranges_map, M.lookup (entityKey val2) year_ranges_map) of
        (Just (_, n), Just (_, m)) -> compare m n
        (Just _,      Nothing)     -> LT
        (Nothing,     Just _)      -> GT
        _                          -> EQ
