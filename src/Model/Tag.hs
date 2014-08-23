module Model.Tag where

import Import

import qualified Data.Map           as M
import           Database.Esqueleto

-- | Get all tags.
fetchAllTagsDB :: YesodDB App [Entity Tag]
fetchAllTagsDB = selectList [] []

-- | Get a map of TagId to the number of Resources with that tag.
fetchTagCountsDB :: YesodDB App (Map TagId Int)
fetchTagCountsDB = fmap (M.fromList . map fromValue) sel
  where
    sel :: YesodDB App [(Value TagId, Value Int)]
    sel = select $
          from $ \rt -> do
          groupBy (rt^.ResourceTagTagId)
          return (rt^.ResourceTagTagId, countRows)

-- | Get the year range of all Resources of with a specific Tag. If none of the
-- Resources with that Tag have any published year, then the Tag will not exist
-- in the returned map.
fetchTagYearRangesDB :: YesodDB App (Map TagId (Int, Int))
fetchTagYearRangesDB = fmap (foldr f mempty) $
    select $
    from $ \(r `InnerJoin` rt) -> do
    on (r^.ResourceId ==. rt^.ResourceTagResId)
    groupBy (rt^.ResourceTagTagId)
    return (rt^.ResourceTagTagId, min_ (r^.ResourcePublished), max_ (r^.ResourcePublished))
  where
    f :: (Value TagId, Value (Maybe (Maybe Int)), Value (Maybe (Maybe Int)))
      -> Map TagId (Int, Int)
      -> Map TagId (Int, Int)
    f (Value _,       Value (Just Nothing),  Value (Just Nothing))  = id
    f (Value tag_id, Value (Just (Just m)), Value Nothing)         = M.insert tag_id (m, m)
    f (Value tag_id, Value Nothing,         Value (Just (Just m))) = M.insert tag_id (m, m)
    f (Value tag_id, Value (Just (Just m)), Value (Just (Just n))) = M.insert tag_id (m, n)
    f (_,             Value Nothing,         Value Nothing)         = id
    -- How could min_ return NULL but max not, or vice versa?
    f (_, _, _) = error "fetchAuthorYearRangeDB: incorrect assumption about return value of min_/max_"
