module Model.Tag where

import Import

import Model.Resource

-- | Get all tags.
fetchAllTagsDB :: YesodDB App [Entity Tag]
fetchAllTagsDB = selectList [] []

-- | Get a map of TagId to the number of Resources with that tag.
fetchTagCountsDB :: YesodDB App (Map TagId Int)
fetchTagCountsDB = fetchResourceFieldCountsDB ResourceTagTagId

-- | Get the year range of all Resources of with a specific Tag. If none of the
-- Resources with that Tag have any published year, then the Tag will not exist
-- in the returned map.
fetchTagYearRangesDB :: YesodDB App (Map TagId (Int, Int))
fetchTagYearRangesDB = fetchResourceFieldYearRangesDB ResourceTagResId ResourceTagTagId
