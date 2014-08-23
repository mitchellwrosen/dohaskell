module Model.Author where

import Import

import           Model.Utils

import qualified Data.Map           as M
import           Database.Esqueleto

-- | Get all authors, sorted alphabetically (ignore case).
fetchAllAuthorsDB :: YesodDB App [Entity Author]
fetchAllAuthorsDB = getAllEntities (alphabeticIgnoreCase authorName)

-- | Get a map of AuthorId to the number of Resources with that Author.
fetchAuthorResourceCountsDB :: YesodDB App (Map AuthorId Int)
fetchAuthorResourceCountsDB = fmap (M.fromList . map fromValue) $
    select $
    from $ \ra -> do
    groupBy (ra^.ResAuthorAuthId)
    return (ra^.ResAuthorAuthId :: SqlExpr (Value AuthorId), countRows :: SqlExpr (Value Int))

-- | Get the year range of all Resources of an Author. If none of the Author's Resources
-- have any published year, then the AuthorId will not exist in the returned map.
fetchAuthorYearRangesDB :: YesodDB App (Map AuthorId (Int, Int))
fetchAuthorYearRangesDB = fmap (foldr f mempty) $
    select $
    from $ \(r `InnerJoin` ra) -> do
    on (r^.ResourceId ==. ra^.ResAuthorResId)
    groupBy (ra^.ResAuthorAuthId)
    return (ra^.ResAuthorAuthId, min_ (r^.ResourcePublished), max_ (r^.ResourcePublished))
  where
    f :: (Value AuthorId, Value (Maybe (Maybe Int)), Value (Maybe (Maybe Int)))
      -> Map AuthorId (Int, Int)
      -> Map AuthorId (Int, Int)
    f (Value _,       Value (Just Nothing),  Value (Just Nothing))  = id
    f (Value auth_id, Value (Just (Just m)), Value Nothing)         = M.insert auth_id (m, m)
    f (Value auth_id, Value Nothing,         Value (Just (Just m))) = M.insert auth_id (m, m)
    f (Value auth_id, Value (Just (Just m)), Value (Just (Just n))) = M.insert auth_id (m, n)
    f (_,             Value Nothing,         Value Nothing)         = id
    -- How could min_ return NULL but max not, or vice versa?
    f (_, _, _) = error "fetchAuthorYearRangeDB: incorrect assumption about return value of min_/max_"
