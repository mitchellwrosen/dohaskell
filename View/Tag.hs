module View.Tag where

import Import

tagListItemWidget :: Entity Tag -> Int ->  Widget
tagListItemWidget (Entity _ (Tag tag)) total = do
  [whamlet|
    <div title="Total resources" .tag-col .tag-count>#{show total}
    <a .tag-col .tag-link href=@{TagR tag}>#{tag}
  |]

-- Like tagListItemWidget', but the user is logged in, so display the number
-- of grokked resources in addition to the total.
tagListItemWidget' :: Entity Tag -> Int -> Int -> Widget
tagListItemWidget' (Entity _ (Tag tag)) total grokked = do
  [whamlet|
    <div title="Grokked resources / total resources" .tag-col .tag-count>#{show total}/#{show grokked}
    <a .tag-col .tag-link href=@{TagR tag}>#{tag}
  |]
