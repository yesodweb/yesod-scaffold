module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , module Prelude
    , module Test.Hspec
    , runDB
    , withApp
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Prelude
import Application (makeFoundation)
import Yesod.Default.Config2 (loadAppSettings, EnvUsage (IgnoreEnv))
import Test.Hspec

import Foundation
import Model

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
