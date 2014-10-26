module Model.List
    ( addListItemDB
    , deleteListItemDB
    , fetchGrokkedListIdDB
    , fetchListLengthDB
    , fetchListResourcesDB
    ) where

import Import

import           Database.Persist.Class.Extra

import           Database.Esqueleto
import qualified Database.Persist   as P

-- | Add a Resource to a User's List. Creates the List if it doesn't already
-- exist. No-op if the Resource is already in the List.
addListItemDB :: UserId -> Text -> ResourceId -> YesodDB App ()
addListItemDB user_id list_name res_id =
    insertBy' (List list_name) >>= \list_id ->
        liftIO getCurrentTime >>= void . insertUnique . ListItem user_id list_id res_id

-- | Delete a Resource from a User's List. Deletes the List if it no longer has
-- any Resources. No-op if the Resource is not in the List.
deleteListItemDB :: UserId -> Text -> ResourceId -> YesodDB App ()
deleteListItemDB user_id list_name res_id =
    getBy (UniqueList list_name) >>= maybe (return ()) (go . entityKey)
  where
    go :: ListId -> YesodDB App ()
    go list_id = do
        deleteBy (UniqueListItem user_id list_id res_id)
        fetchListLengthDB list_id >>= \case
            0 -> P.delete list_id
            _ -> return ()

-- | Get all Resources that belong to a User's List.
fetchListResourcesDB :: UserId -> Text -> YesodDB App [Entity Resource]
fetchListResourcesDB user_id list_name =
    select $
    from $ \(l `InnerJoin` li `InnerJoin` r) -> do
    on (li^.ListItemResId ==. r^.ResourceId)
    on (l^.ListId ==. li^.ListItemListId)
    where_ $
        l^.ListName ==. val list_name &&.
        li^.ListItemUserId ==. val user_id
    return r

fetchGrokkedListIdDB :: YesodDB App (Maybe ListId)
fetchGrokkedListIdDB = fmap entityKey <$> getBy (UniqueList "grokked")

-- | Get the number of Resources added to this List by all Users.
fetchListLengthDB :: ListId -> YesodDB App Int
fetchListLengthDB list_id = fmap (\[Value n] -> n) $
    select $
    from $ \li -> do
    where_ (li^.ListItemListId ==. val list_id)
    return (countRows :: SqlExpr (Value Int))
