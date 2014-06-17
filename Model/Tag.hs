module Model.Tag where

import Import

import           Data.List            (sortBy)
import qualified Data.Map             as M
import           Database.Esqueleto

import Handler.Utils (alphabeticIgnoreCase)

-- Get all tags, sorted alphabetically (ignore case)
getAllTags :: YesodDB App [Entity Tag]
getAllTags = sortBy (alphabeticIgnoreCase tagText) <$> selectList [] []

-- Get a map of TagId to the number of Resources with that tag.
getTagCounts :: YesodDB App (Map TagId Int)
getTagCounts = valsToMap <$>
    (select $
        from $ \rt -> do
        groupBy (rt^.ResourceTagTagId)
        return (rt^.ResourceTagTagId, countRows))
  where
    valsToMap :: [(Value TagId, Value Int)] -> Map TagId Int
    valsToMap = foldr step mempty
      where
        step (Value tagId, Value n) = M.insert tagId n
