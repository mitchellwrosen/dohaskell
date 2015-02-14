{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import           Settings

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Monad.Logger                 (LoggingT, runLoggingT)
import           Control.Monad.Reader                 (ReaderT)
import           Control.Monad.Trans.Resource         (ResourceT, runResourceT)
import           Data.Default                         (def)
import           Data.List                            (sort)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Database.Persist
import           Database.Persist.Sql                 hiding (LogFunc)
import           Network.HTTP.Client.Conduit          (newManager)
import           Network.Wai.Logger                   (clockDateCacher)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Network.Wai.Middleware.RequestLogger ( mkRequestLogger, outputFormat, OutputFormat (..)
                                                      , IPAddrSource (..), destination
                                                      )
import           Prelude                              (last)
import           System.Directory                     (getDirectoryContents)
import           System.Log.FastLogger                (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import           Yesod.Core.Types                     (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main
import           Yesod.Fay

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.About
import Handler.Api.Resource
import Handler.Browse
import Handler.EditResourceRequest
import Handler.FayCommand
import Handler.Feed
import Handler.ReqEditsHub
import Handler.Resource
import Handler.Submit
import Handler.User

import View.Navbar (navbarWidget)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager           <- newManager
    s                 <- staticSite
    dbconf            <- withYamlEnvironment "config/sqlite.yml" (appEnv conf) Database.Persist.loadConfig >>=
                            Database.Persist.applyEnv
    pool              <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet'        <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    let logger     = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s pool manager dbconf logger navbarWidget fayCommandHandler

    -- Perform database migration using our application's logging settings.
    let migration = runResourceT $ do
            Database.Persist.runPool dbconf doMigration pool

    runLoggingT
        migration
        (messageLoggerSource foundation logger)

    return foundation

runRawQuery :: (RawSql a, MonadIO m) => Text -> ReaderT SqlBackend m [a]
runRawQuery = flip rawSql []

runRawStmt :: MonadIO m => Text -> ReaderT SqlBackend m ()
runRawStmt = flip rawExecute []

doMigration :: SqlPersistT (ResourceT (LoggingT IO)) ()
doMigration = do
    createDatabaseVersionIfNotExists
    runOldMigrationFiles >>= createAndRunNewMigrationFiles
  where
    createDatabaseVersionIfNotExists :: SqlPersistT (ResourceT (LoggingT IO)) ()
    createDatabaseVersionIfNotExists = void $
        runRawStmt "CREATE TABLE IF NOT EXISTS \"database_version\" (last_migration INTEGER NOT NULL);"

    -- | Run all necessary migrations, and return the next migration number to create.
    runOldMigrationFiles :: SqlPersistT (ResourceT (LoggingT IO)) Int
    runOldMigrationFiles = do
        last_migration <- getLastMigration
        liftIO (getMigrationNumbers last_migration) >>= \case
            [] -> return (last_migration + 1)
            nums -> do
                forM_ nums $ \num -> do
                    let file = toSafeMigrateFile num
                    $(logInfo) $ "Running existing migration: " <> T.pack file
                    liftIO (T.lines <$> T.readFile file) >>= mapM_ runRawStmt
                let new_last_migration = last nums
                updateDatabaseVersion new_last_migration
                return (new_last_migration + 1)

    -- | Get the last migration Int from the database_version table.
    getLastMigration :: SqlPersistT (ResourceT (LoggingT IO)) Int
    getLastMigration = do
        last_migration <- runRawQuery "SELECT last_migration FROM database_version;"
        case last_migration of
            [] -> do
                runRawStmt "INSERT INTO database_version values (0);"
                return 0
            [Single n] -> return n
            _ -> error "Application.getLastMigration: not reached"

    -- | Get the migration numbers that need to be run.
    getMigrationNumbers :: Int -> IO [Int]
    getMigrationNumbers n =
        sort .
          filter (> n) .
            map fromMigrateFile .
              filter (`notElem` [".",".."])
                <$> getDirectoryContents "migrations"

    -- | Create zero or more new migration files. Run them only if they're all safe
    -- (in which case there would be only one).
    createAndRunNewMigrationFiles :: Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
    createAndRunNewMigrationFiles next_migration = do
        migrations <- addSemicolons <$> parseMigration' migrateAll
        writeMigrations migrations next_migration

        unless (null migrations) $
            -- True means unsafe, so if any True, return False. Only run if all are safe,
            -- in which case we know we only made one new migration file with number
            -- next_migration.
            if not (any fst migrations)
                then runMigrations next_migration (map snd migrations)
                else error "Aborting due to unsafe migrations."
      where
        addSemicolons :: CautiousMigration -> CautiousMigration
        addSemicolons = map (\(b,s) -> (b, s `T.snoc` ';'))

        writeMigrations :: CautiousMigration -> Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
        writeMigrations = writeSafeMigrations

        writeSafeMigrations :: CautiousMigration -> Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
        writeSafeMigrations = writeMigrations' (not . fst) toSafeMigrateFile writeUnsafeMigrations

        writeUnsafeMigrations :: CautiousMigration -> Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
        writeUnsafeMigrations = writeMigrations' fst toUnsafeMigrateFile writeSafeMigrations

        writeMigrations'
            -- Predicate on single migration statement ("is safe" or "is unsafe")
            :: ((Bool,Sql) -> Bool)
            -- Migration filepath maker.
            -> (Int -> FilePath)
            -- The other guy (safe, if this is unsafe; unsafe, if this is safe)
            -> (CautiousMigration -> Int -> SqlPersistT (ResourceT (LoggingT IO)) ())
            -> CautiousMigration -> Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
        writeMigrations' _ _ _ [] _ = return ()
        writeMigrations' p toMigrateFile writeOtherMigrations ms n = do
            let (ms',rest) = span p ms
            if null ms'
                then writeOtherMigrations rest n
                else do
                    let file = toMigrateFile n
                    $(logInfo) $ "Writing migration: " <> T.pack file <> " (" <> T.pack (show $ length ms') <> " statements)"
                    liftIO $ T.writeFile file (T.unlines (map snd ms'))
                    writeOtherMigrations rest (n+1)

        runMigrations :: Int -> [Sql] -> SqlPersistT (ResourceT (LoggingT IO)) ()
        runMigrations n stmts = do
            $(logInfo) $ "Running new migration: " <> T.pack (toSafeMigrateFile n)
            updateDatabaseVersion n
            mapM_ runRawStmt stmts

    updateDatabaseVersion :: Int -> SqlPersistT (ResourceT (LoggingT IO)) ()
    updateDatabaseVersion n = runRawStmt ("UPDATE database_version SET last_migration = " <> T.pack (show n) <> ";")

    -- | 10 -> "migrations/10.sql"
    toSafeMigrateFile :: Int -> FilePath
    toSafeMigrateFile = (\s -> "migrations/" <> s <> ".sql") . show

    -- | 10 -> "migrations/unsafe-10.sql" (intentionally fails parsing by fromMigrateFile)
    toUnsafeMigrateFile :: Int -> FilePath
    toUnsafeMigrateFile = (\s -> "migrations/unsafe-" <> s <> ".sql") . show

    -- | "10.sql" -> 10
    fromMigrateFile :: FilePath -> Int
    fromMigrateFile s = case (reads . takeWhile (/= '.')) s of
        [(n,"")] -> n
        _ -> error $ "Unsafe migration: migrations/" ++ s

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
