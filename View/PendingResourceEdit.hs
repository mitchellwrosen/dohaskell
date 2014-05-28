module View.PendingResourceEdit where

import Import

import           Data.Set (Set)
import qualified Data.Set as S

import Model.PendingResourceEdit (getPendingEditTags)
import Model.Resource            (getResourceTags)

data TagStatus = Added | Removed | Sustained

pendingResourceEditWidget :: ResourceId -> Entity PendingResourceEdit -> Widget
pendingResourceEditWidget resId (Entity eid edit) = do
    res <- handlerToWidget . runDB $ get404 resId

    let isNewTitle = resourceTitle res /= pendingResourceEditTitle edit
    let isNewType  = resourceType  res /= pendingResourceEditType  edit

    oldTags <- S.fromList . map tagText <$> handlerToWidget (runDB $ getResourceTags resId)
    newTags <- S.fromList <$> handlerToWidget (runDB $ getPendingEditTags eid)
    let allTags = makeAllTags oldTags newTags (oldTags `S.union` newTags)

    [whamlet|
        <div :isNewTitle:style="font-weight:bold">#{pendingResourceEditTitle edit}
        <div :isNewType:style="font-weight:bold">#{descResourceType $ pendingResourceEditType edit}
        <ul>
            $forall (tag, status) <- allTags
                <li>
                    $case status
                        $of Added
                            <div style="font-weight:bold">#{tag}
                        $of Removed
                            <div style="text-decoration:line-through">#{tag}
                        $of Sustained
                            <div>#{tag}
        <form method=post>
           <input type=submit formaction=@{ReqEditAcceptR eid} value=Accept>
           <input type=submit formaction=@{ReqEditDeclineR eid} value=Decline>
    |]

-- Using old tags, new tags, and combined tags, annotate each combined tag.
makeAllTags :: Set Text -> Set Text -> Set Text -> [(Text, TagStatus)]
makeAllTags oldTags newTags = S.foldr step []
  where
    step :: Text -> [(Text, TagStatus)] -> [(Text, TagStatus)]
    step tag = ((tag, status) :)
      where
        status = if S.member tag oldTags
                     then if S.member tag newTags
                              then Sustained
                              else Removed
                     else Added
