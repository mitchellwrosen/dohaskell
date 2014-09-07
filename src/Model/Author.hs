module Model.Author where

import Import

import Model.Resource

-- | Get all authors.
fetchAllAuthorsDB :: YesodDB App [Entity Author]
fetchAllAuthorsDB = selectList [] []

-- | Get a map of AuthorId to the number of Resources with that Author.
fetchAuthorResourceCountsDB :: YesodDB App (Map AuthorId Int)
fetchAuthorResourceCountsDB = fetchResourceFieldCountsDB ResAuthorAuthId

-- | Get the year range of all Resources of an Author. If none of the Author's Resources
-- have any published year, then the AuthorId will not exist in the returned map.
fetchAuthorYearRangesDB :: YesodDB App (Map AuthorId (Int, Int))
fetchAuthorYearRangesDB = fetchResourceFieldYearRangesDB ResAuthorResId ResAuthorAuthId
