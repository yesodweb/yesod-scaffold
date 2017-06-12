{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import Application (makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Database.Persist as X hiding (get)
import Database.Persist.Sql
       (SqlBackend, SqlPersistM, connEscapeName, rawExecute, rawSql,
        runSqlPersistMPool, unSingle)
import Foundation as X
import Model as X
import Test.Hspec as X
import Text.Shakespeare.Text (st)
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp =
    before $ do
        settings <-
            loadYamlSettings
                ["config/test-settings.yml", "config/settings.yml"]
                []
                useEnv
        foundation <- makeFoundation settings
        wipeDB foundation
        logWare <- liftIO $ makeLogWare foundation
        return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app =
    runDBWithApp app $ do
        tables <- getTables
        sqlBackend <- ask
        let escapedTables = map (connEscapeName sqlBackend . DBName) tables
            query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
        rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <-
        rawSql
            [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |]
            []
    return $ map unSingle tables

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = do
    runDB $ insertEntity User {userIdent = ident, userPassword = Nothing}
