module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    runDBWithApp app $ do
        tables <- getTables
        let quotedTables = map (\s -> "\"" ++ s ++ "\"") tables -- Words like 'user' are reserved words Postgres, so a query like `TRUNCATE TABLE user` needs to be written as `TRUNCATE TABLE "user"`.
            query = "TRUNCATE TABLE " ++ (intercalate ", " quotedTables)
        rawExecute query []
    return ()

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';" []
    return $ map unSingle tables