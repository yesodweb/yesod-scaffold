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
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

wipeDB :: App -> IO ()
wipeDB foundation = do
    let pool = appConnPool foundation
    flip runSqlPersistMPool pool $ do
        tables <- getTables
        let queries = map ("TRUNCATE TABLE " ++ ) tables

        rawExecute "SET foreign_key_checks = 0;" []
        forM_ queries (\q -> rawExecute q [])
        rawExecute "SET foreign_key_checks = 1;" []
    return ()

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SHOW TABLES;" []
    return $ map unSingle tables
