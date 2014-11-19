{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , makeFoundation
    , loadAppSettings
    ) where

import Import
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
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import Safe (readMay)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setOnException, defaultShouldDisplayException)
import Control.Monad (when)
import Control.Monad.Logger (liftLoc)
import Language.Haskell.TH.Syntax (qLocation)

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
            if appDevelopment $ appSettings foundation
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

loadAppSettings :: Bool -- ^ for tests?
                -> IO AppSettings
loadAppSettings _tests = return AppSettings -- FIXME
    { appDevelopment = compileTimeDevelopment
    , appStaticDir = compileTimeStaticDir
    , appPostgresConf = error "appPostgresConf"
    , appRoot = "http://localhost:3000"
    , appHost = "*4"
    , appPort = 3000
    , appCopyright = "Insert copyright statement here"
    , appAnalytics = Nothing
    }

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
    settings <- loadAppSettings False
    (app, _logFunc) <- makeApplication settings

    env <- getEnvironment
    let p = fromMaybe (appPort settings) $ lookup "PORT" env >>= readMay
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMay
    putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay

    return (p, app)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    settings <- loadAppSettings False
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
