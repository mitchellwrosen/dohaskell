module Model.ResourceTag
    ( getResourceTagsByResId
    ) where

import Import

import Database.Esqueleto

getResourceTagsByResId :: ResourceId -> YesodDB App [Entity ResourceTag]
getResourceTagsByResId resId =
    select $
        from $ \rt -> do
        where_ (rt^.ResourceTagResId ==. val resId)
        return rt
