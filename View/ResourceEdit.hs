module View.ResourceEdit where

import Import

import Model.Resource (getResourceTags)

editTitleWidget :: Entity Resource -> Entity EditTitle -> Widget
editTitleWidget (Entity _ res) (Entity editId edit) =
    [whamlet|
        <div>
            <a href=#{resourceUrl res}>link
        <span style="text-decoration:line-through">#{resourceTitle res}
        <span style="font-weight:bold">#{editTitleTitle edit}
        <div>#{descResourceType $ resourceType res}
        <form method=post>
            <input type=submit formaction=@{EditTitleAcceptR editId} value=Accept>
            <input type=submit formaction=@{EditTitleDeclineR editId} value=Decline>
    |]

editTypeWidget :: Entity Resource -> Entity EditType -> Widget
editTypeWidget (Entity _ res) (Entity editId edit) =
    [whamlet|
        <div>
            <a href=#{resourceUrl res}>link
        <div>#{resourceTitle res}
        <span style="text-decoration:line-through">#{descResourceType $ resourceType res}
        <span style="font-weight:bold">#{descResourceType $ editTypeType edit}
        <form method=post>
            <input type=submit formaction=@{EditTypeAcceptR editId} value=Accept>
            <input type=submit formaction=@{EditTypeDeclineR editId} value=Decline>
    |]

editAddTagWidget :: Entity Resource -> Entity EditAddTag -> Widget
editAddTagWidget (Entity resId res) (Entity editId edit) = do
    tags <- handlerToWidget . runDB $ getResourceTags resId
    [whamlet|
        <div>
            <a href=#{resourceUrl res}>link
        <div>#{resourceTitle res}
        <div>#{descResourceType $ resourceType res}
        $forall Tag text <- tags
            <span>#{text},
        <span style="font-weight:bold; color:green">#{editAddTagText edit}
        <form method=post>
            <input type=submit formaction=@{EditAddTagAcceptR editId} value=Accept>
            <input type=submit formaction=@{EditAddTagDeclineR editId} value=Decline>
    |]

editRemoveTagWidget :: Entity Resource -> Entity EditRemoveTag -> Widget
editRemoveTagWidget (Entity resId res) (Entity editId edit) = do
    tags <- handlerToWidget . runDB $ getResourceTags resId
    [whamlet|
        <div>
            <a href=#{resourceUrl res}>link
        <div>#{resourceTitle res}
        <div>#{descResourceType $ resourceType res}
        $forall Tag text <- tags
            $if text == editRemoveTagText edit
                <span style="font-weight:bold; color:red; text-decoration:line-through">#{editRemoveTagText edit},
            $else
                <span>#{text},
        <form method=post>
            <input type=submit formaction=@{EditRemoveTagAcceptR editId} value=Accept>
            <input type=submit formaction=@{EditRemoveTagDeclineR editId} value=Decline>
    |]
