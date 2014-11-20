module TestImport
    ( module TestImport
    , module X
    ) where

import Yesod.Test as X
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Prelude as X
import Application (makeFoundation)
import Yesod.Default.Config2 (loadAppSettings, EnvUsage (IgnoreEnv))
import Test.Hspec as X

import Foundation as X
import Model as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        IgnoreEnv
    makeFoundation settings
