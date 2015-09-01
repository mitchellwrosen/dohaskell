module Foundation where

import Import.NoFoundation
import Database.Persist.Sql        (ConnectionPool, runSqlPool)
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Yesod.Auth.BrowserId        (authBrowserId)
import Yesod.Auth.Message          (AuthMessage (InvalidLogin))
import Yesod.Default.Util          (addStaticContentExternal)
import Yesod.Core.Types            (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appNavbar      :: WidgetT App IO ()
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = Just <$> defaultClientSessionBackend timeoutMins keyFile
      where timeoutMins = 10080 -- 1 week
            keyFile     = "config/client_session_key.aes"

    defaultLayout innerWidget = do
        mmsg <- getMessage
        navbarWidget <- appNavbar <$> getYesod

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    -- Gave up trying to use this function because Foundation can't import
    -- anything that imports Import (which is everything).
    isAuthorized _ _ = return Authorized

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

requiresAuthorization :: Handler AuthResult
requiresAuthorization = maybe AuthenticationRequired (const Authorized) <$> maybeAuthId

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = getYesod >>= runSqlPool action . appConnPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR
    redirectToReferer _ = True

    authenticate creds = runDB $
        getBy (UniqueUserName $ credsIdent creds) >>= \case
            Just (Entity uid _) -> pure (Authenticated uid)
            Nothing ->
                liftIO getCurrentTime
                >>= insert . User (credsIdent creds) "anonymous" False
                >>= pure . Authenticated

    authPlugins _ = [dohaskellAuthBrowserId]
      where
        dohaskellAuthBrowserId = (authBrowserId def) { apLogin = apLogin' }
        apLogin' toMaster = do
          apLogin (authBrowserId def) toMaster
          [whamlet|
            <p>
              <a href="http://en.wikipedia.org/wiki/Mozilla_Persona">Mozilla Persona
              is a secure authentication system with a focus on privacy.
              You may use any e-mail address to log in.
          |]
          toWidget [cassius|
            p
              font-size: 22px
              line-height: 25px
              margin: 0px auto 20px auto
              text-align: justify
              width: 30em
          |]
    authHttpManager = appHttpManager

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
