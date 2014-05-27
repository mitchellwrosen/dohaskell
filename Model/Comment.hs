module Model.Comment where

import Import

import Data.Time (getCurrentTime)

makeComment :: ResourceId -> UserId -> Textarea -> Handler ()
makeComment resId uid text = do
    now <- liftIO getCurrentTime
    void $
        runDB $ insert $ Comment resId uid now text
