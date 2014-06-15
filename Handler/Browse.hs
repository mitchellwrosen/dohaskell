module Handler.Browse where

import Import

import Database.Persist.Sql

import View.Resource (resourceListWidget)

getBrowseR :: Handler Html
getBrowseR = do
    -- TODO: Sort in Haskell rather than in database?
    resources <- runDB $ rawSql "SELECT * FROM resource ORDER BY title COLLATE NOCASE ASC;" []

    defaultLayout $
      resourceListWidget resources "dohaskell | browse"
