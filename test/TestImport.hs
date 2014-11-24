module TestImport
    ( module TestImport
    , module X
    ) where

import Yesod.Test as X
import Database.Persist as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import ClassyPrelude as X
import Application (makeFoundation)
import Yesod.Default.Config2 (loadAppSettings, ignoreEnv)
import Test.Hspec as X
import Settings

import Foundation as X
import Model as X

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
