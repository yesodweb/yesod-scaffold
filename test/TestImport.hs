module TestImport
    ( module TestImport
    , module X
    ) where

import Application              (makeFoundation)
import ClassyPrelude            as X
import Database.Persist         as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import Foundation               as X
import Model                    as X
import Settings                 (appDatabaseConf)
import Test.Hspec               as X
import Yesod.Default.Config2    (ignoreEnv, loadAppSettings)
import Yesod.Test               as X
-- Wiping the test database
import Database.MongoDB.Query (allCollections)
import Database.MongoDB.Admin (dropCollection)
import Control.Monad.Trans.Control (MonadBaseControl)

runDB :: Action IO a -> YesodExample App a
runDB action = do
    master <- getTestYesod
    liftIO $ runMongoDBPool
        (mgAccessMode $ appDatabaseConf $ appSettings master)
        action
        (appConnPool master)

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    app <- makeFoundation settings
    wipeDB app
    return app

wipeDB :: App -> IO ()
wipeDB app = do
    void $ runMongoDBPool
        (mgAccessMode $ appDatabaseConf $ appSettings app)
        (dropAllCollections)
        (appConnPool app)

dropAllCollections :: (MonadIO m, MonadBaseControl IO m) => Action m [Bool]
dropAllCollections = allCollections >>= return . filter (not . isSystemCollection) >>= mapM dropCollection
      where
        isSystemCollection = isPrefixOf "system."

