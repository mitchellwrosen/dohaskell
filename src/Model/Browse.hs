-- TODO: delete this module
module Model.Browse
    ( getTypeCounts
    ) where

import Import

import Model.Resource
import Model.Utils

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
