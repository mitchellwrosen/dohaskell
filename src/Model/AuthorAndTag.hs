module Model.AuthorAndTag
    ( getAllAuthors
    , getAllTags
    , getAuthorCounts
    , getTagCounts
    ) where

import Import

import           Handler.Utils        (alphabeticIgnoreCase)
import           Model.Utils          (getAllEntities)

import qualified Data.Map             as M
import           Database.Esqueleto

-- | Get all authors, sorted alphabetically (ignore case).
getAllAuthors :: YesodDB App [Entity Author]
getAllAuthors = getAllEntities (alphabeticIgnoreCase authorName)

-- | Get all tags, sorted alphabetically (ignore case).
getAllTags :: YesodDB App [Entity Tag]
getAllTags = getAllEntities (alphabeticIgnoreCase tagTag)

-- | Get a map of AuthorId to the number of Resources with that Author.
getAuthorCounts :: YesodDB App (Map AuthorId Int)
getAuthorCounts = fmap valsToMap $
    select $
    from $ \ra -> do
    groupBy (ra^.ResAuthorAuthId)
    return (ra^.ResAuthorAuthId, countRows)
  where
    valsToMap :: [(Value AuthorId, Value Int)] -> Map AuthorId Int
    valsToMap = foldr step mempty
      where
        step (Value author_id, Value n) = M.insert author_id n

-- | Get a map of TagId to the number of Resources with that tag.
getTagCounts :: YesodDB App (Map TagId Int)
getTagCounts = fmap valsToMap $
    select $
    from $ \rt -> do
    groupBy (rt^.ResourceTagTagId)
    return (rt^.ResourceTagTagId, countRows)
  where
    valsToMap :: [(Value TagId, Value Int)] -> Map TagId Int
    valsToMap = foldr step mempty
      where
        step (Value tag_id, Value n) = M.insert tag_id n
