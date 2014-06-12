module Model.Tag where

import Import

import           Data.List            (sortBy)
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Database.Esqueleto

-- Get all tags, sorted alphabetically (ignore case)
getAllTags :: YesodDB App [Entity Tag]
getAllTags = sortBy alphabeticIgnoreCase <$> selectList [] []
  where
    -- T.head is safe here because tags can't be empty.
    alphabeticIgnoreCase :: Entity Tag -> Entity Tag -> Ordering
    alphabeticIgnoreCase (Entity _ (Tag t1)) (Entity _ (Tag t2)) = T.toLower t1 `compare` T.toLower t2

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
