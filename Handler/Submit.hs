module Handler.Submit where

import Import

import Data.Time       (getCurrentTime)
import Yesod.Auth

getSubmitR :: Handler Html
getSubmitR = do
    uid <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost (resourceForm uid)
    defaultLayout $ do
        setTitle "Submit a resource"
        $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
    ((result, formWidget), formEnctype) <- runFormPost . resourceForm =<< requireAuthId
    postSubmitR' result formWidget formEnctype

postSubmitR' :: FormResult (Resource, [Tag]) -> Widget -> Enctype -> Handler Html
postSubmitR' result formWidget formEnctype =
    case result of
        FormSuccess (res,tags) -> runDB (insertBy res) >>= either onDuplicate onNew
        _ -> do
            setMessage "TODO: form failure"
            defaultLayout $ do
                setTitle "Submit a resource"
                $(widgetFile "submit")
  where
    onDuplicate :: Entity Resource -> Handler Html
    onDuplicate (Entity resId _) = do
        setDuplicateUrlMessage resId
        defaultLayout $ do
            setTitle "Submit a resource"
            $(widgetFile "submit")

    onNew :: ResourceId -> Handler Html
    onNew _ = do
        setMessage "Thanks for your submission!"
        redirect HomeR

    setDuplicateUrlMessage :: ResourceId -> Handler ()
    setDuplicateUrlMessage resId = do
        giveUrlRenderer [hamlet|
                URL already exists.
                <a href=@{ResourceR resId}>Take me there.
            |] >>= setMessage

resourceForm :: UserId -> Form (Resource, [Tag])
resourceForm uid = renderDivs $ (,) <$> resourceEntityForm uid <*> tagsForm

resourceEntityForm :: UserId -> AForm Handler Resource
resourceEntityForm uid = Resource
    <$> areq textField "Title" Nothing
    <*> areq urlField "Url" Nothing
    <*> areq (selectFieldList fields) "Type" Nothing
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
  where
    -- TODO: can this be generated with Data.Data?
    fields :: [(Text, ResourceType)]
    fields = [("Blog post", BlogPostResource)
             ,("Lecture"  , LectureResource )
             ,("Tutorial" , TutorialResource)
             ,("Video"    , VideoResource   )
             ]

tagsForm :: AForm Handler [Tag]
tagsForm = undefined --map (Tag . pack) . splitOn "," . unpack <$> areq textField "Tags" Nothing
