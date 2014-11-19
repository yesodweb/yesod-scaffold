{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , loadAppSettings
    ) where

import Import hiding (applyEnv)
import Yesod.Auth
import Yesod.Default.Main
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Network.HTTP.Client.Conduit (newManager)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, toLogStr)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import System.Environment (getEnvironment, getArgs)
import Data.Maybe (fromMaybe)
import Safe (readMay)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setOnException, defaultShouldDisplayException)
import Control.Exception (throwIO)
import Control.Monad (when, forM)
import Control.Monad.Logger (liftLoc)
import Language.Haskell.TH.Syntax (qLocation)
import Data.Yaml (decodeEither', decodeFileEither)
import Data.Aeson (fromJSON, Result (..))
import Yesod.Default.Config2
import System.Directory (doesFileExist)
import Control.Concurrent (forkIO, threadDelay)
import System.Exit (exitSuccess)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppSettings -> IO (Application, LogFunc)
makeApplication settings = do
    foundation <- makeFoundation settings

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Load the settings from the following three sources:
--
-- * Run time config files
--
-- * Run time environment variables
--
-- * The default compile time config file
loadAppSettings :: [FilePath] -- ^ run time config files to use, earlier files have precedence
                -> Bool -- ^ use environment variables
                -> Bool -- ^ use the compile time config file as a base config
                -> IO AppSettings
loadAppSettings runTimeFiles useEnv useCompileConfig = do
    compileValues <-
        if useCompileConfig
            then
                case decodeEither' configSettingsYml of
                    Left e -> do
                        putStrLn "Unable to parse compile-time config/settings.yml as YAML"
                        throwIO e
                    Right value -> return [value]
            else return []
    runValues <- forM runTimeFiles $ \fp -> do
        eres <- decodeFileEither fp
        case eres of
            Left e -> do
                putStrLn $ "Could not parse file as YAML: " ++ fp
                throwIO e
            Right value -> return value
    let value' = getMergedValue $ mconcat $ map MergedValue $ runValues ++ compileValues
    value <-
        if useEnv
            then applyCurrentEnv value'
            else return $ applyEnv mempty value'

    case fromJSON value of
        Error s -> error $ "Could not convert to AppSettings: " ++ s
        Success settings -> return settings

-- | Creates your foundation datatype and performs some initialization.
makeFoundation :: AppSettings -> IO App
makeFoundation settings = do
    manager <- newManager
    static <- staticSite settings

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        mkFoundation pool = App
            { appSettings = settings
            , getStatic = static
            , connPool = pool
            , httpManager = manager
            , appLogger = logger
            }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    let dbconf = appPostgresConf settings
    pool <- flip runLoggingT logFunc
          $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
    let foundation = mkFoundation pool

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc
        (Database.Persist.runPool dbconf (runMigration migrateAll) pool)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
    settings <- loadAppSettings ["config/settings.yml"] True False
    (app, _logFunc) <- makeApplication settings

    env <- getEnvironment
    let p = fromMaybe (appPort settings) $ lookup "PORT" env >>= readMay
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMay
    putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay

    return (p, app)

-- | main function for use by yesod devel
develMain :: IO ()
develMain = do
#ifndef mingw32_HOST_OS
    _ <- installHandler sigINT (Catch $ return ()) Nothing
#endif

    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    _ <- forkIO $ runSettings (setPort port defaultSettings) app
    loop
  where
    loop :: IO ()
    loop = do
        threadDelay 100000
        e <- doesFileExist "yesod-devel/devel-terminate"
        if e then terminateDevel else loop

    terminateDevel :: IO ()
    terminateDevel = exitSuccess

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    runTimeConfigs' <- getArgs
    runTimeConfigs <-
        if null runTimeConfigs'
            then do
                let fp = "config/settings.yml"
                exists <- doesFileExist fp
                if exists
                    then return [fp]
                    else return []
            else return runTimeConfigs'
    settings <- loadAppSettings
        runTimeConfigs
        True -- allow environment variables to override
        True -- fall back to compile-time values, set to False to require values at runtime
    (app, logFunc) <- makeApplication settings
    runSettings
        ( setPort (appPort settings)
        $ setHost (appHost settings)
        $ setOnException (\_req e ->
            when (defaultShouldDisplayException e) $ logFunc
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from Warp: " ++ show e))
          defaultSettings)
        app
