module TestImport
    ( module TestImport
    , module X
    ) where

import Application              (makeFoundation, makeLogWare)
import ClassyPrelude            as X hiding (delete, deleteBy)
import Database.Persist         as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import Foundation               as X
import Model                    as X
import Settings                 (appDatabaseConf)
import Test.Hspec               as X
import Yesod.Default.Config2    (ignoreEnv, loadYamlSettings)
import Yesod.Test               as X
-- Wiping the test database
import Database.MongoDB.Query (allCollections)
import Database.MongoDB.Admin (dropCollection)
import Control.Monad.Trans.Control (MonadBaseControl)

runDB :: Action IO a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
    liftIO $ runMongoDBPool
        (mgAccessMode $ appDatabaseConf $ appSettings app)
        query
        (appConnPool app)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will wipe your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = void $ runDBWithApp app dropAllCollections

dropAllCollections :: (MonadIO m, MonadBaseControl IO m) => Action m [Bool]
dropAllCollections = allCollections >>= return . filter (not . isSystemCollection) >>= mapM dropCollection
      where
        isSystemCollection = isPrefixOf "system."

