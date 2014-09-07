module Model.Collection where

import Import

import Model.Resource

-- | Get all Collections.
fetchAllCollectionsDB :: YesodDB App [Entity Collection]
fetchAllCollectionsDB = selectList [] []

-- | Get a map of CollectionId to the number of Resources in that Collection
fetchCollectionResourceCountsDB :: YesodDB App (Map CollectionId Int)
fetchCollectionResourceCountsDB = fetchResourceFieldCountsDB ResCollectionColId

-- | Get the year range of all Resources of with a specific Tag. If none of the
-- Resources with that Tag have any published year, then the Tag will not exist
-- in the returned map.
fetchCollectionYearRangesDB :: YesodDB App (Map CollectionId (Int, Int))
fetchCollectionYearRangesDB = fetchResourceFieldYearRangesDB ResCollectionResId ResCollectionColId
