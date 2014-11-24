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
    makeFoundation settings
