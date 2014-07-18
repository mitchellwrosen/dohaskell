module Handler.Utils
    ( denyPermissionIfDifferentUser
    , denyPermissionIfDoesntHaveAuthorityOver
    , denyPermissionIfNotAdmin
    , prettyAgo
    ) where

import Import

import           Model.User (isAdministrator, userHasAuthorityOver)

import qualified Data.Text  as T
import           Data.Time

denyPermissionIfDifferentUser :: UserId -> Handler ()
denyPermissionIfDifferentUser requestedUser = maybeAuthId >>= \case
    Nothing -> deny
    Just thisUser ->
        runDB (get requestedUser) >>= \case
            Nothing -> notFound
            Just _  -> when (requestedUser /= thisUser)
                           deny

denyPermissionIfDoesntHaveAuthorityOver :: UserId -> Handler ()
denyPermissionIfDoesntHaveAuthorityOver nerd = maybeAuthId >>= \case
    Nothing -> deny
    Just bully ->
        runDB (get nerd) >>= \case
            Nothing -> notFound
            Just _  -> do
                ok <- runDB $ userHasAuthorityOver bully nerd
                when (not ok)
                    deny

denyPermissionIfNotAdmin :: Handler ()
denyPermissionIfNotAdmin = maybeAuthId >>= \case
    Nothing  -> deny
    Just uid -> runDB (isAdministrator uid) >>= \b -> unless b deny

deny :: Handler ()
deny = permissionDenied "You don't have permission to view this page."

-- | Pretty-print a time "ago".
-- TODO: Find a better home for this function.
prettyAgo :: (Functor m, MonadIO m) => UTCTime -> m Text
prettyAgo t = do
  secs_ago <- round . flip diffUTCTime t <$> liftIO getCurrentTime
  if | secs_ago < secsPerWeek  -> return $ prettyAgo' secsPerDay   "today"      " day"   secs_ago
     | secs_ago < secsPerMonth -> return $ prettyAgo' secsPerWeek  "last week"  " week"  secs_ago
     | secs_ago < secsPerYear  -> return $ prettyAgo' secsPerMonth "last month" " month" secs_ago
     | otherwise               -> return $ prettyAgo' secsPerYear  "last year"  " year"  secs_ago
  where
    prettyAgo' :: Integer -> Text -> Text -> Integer -> Text
    prettyAgo' divisor if_one_text if_many_text secs_ago =
        case secs_ago `div` divisor of
            0 -> if_one_text
            n -> T.pack (show n) <> plural n if_many_text <> " ago"

    plural :: Integer -> Text -> Text
    plural 1 = id
    plural _ = flip T.snoc 's'

    secsPerDay, secsPerWeek, secsPerMonth, secsPerYear :: Integer
    secsPerDay   = 86400    -- 60*60*24
    secsPerWeek  = 604800   -- 60*60*24*7
    secsPerMonth = 2592000  -- 60*60*24*30
    secsPerYear  = 31536000 -- 60*60*24*365
