{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application              (makeFoundation, makeLogWare)
import ClassyPrelude            as X hiding (delete, deleteBy, Handler)
import Database.Persist         as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import Foundation               as X
import Model                    as X
import Settings                 (appDatabaseConf)
import Test.Hspec               as X
import Yesod.Default.Config2    (useEnv, loadYamlSettings)
import Yesod.Auth               as X
import Yesod.Test               as X
-- Wiping the test database
import Database.MongoDB.Query (allCollections)
import Database.MongoDB.Admin (dropCollection)
import Control.Monad.Trans.Control (MonadBaseControl)

import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

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

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
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

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = runDB $ do
    user <- insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        }
    _ <- insert Email
        { emailEmail = ident
        , emailUserId = Just $ entityKey user
        , emailVerkey = Nothing
        }
    return user
