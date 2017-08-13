import Import

import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn)
import Control.Monad.Logger (runStderrLoggingT)

insertUser :: MonadIO m => (Text, Text) -> ReaderT SqlBackend m ()
insertUser (ident, pass) = do
  insert_ $ User ident (Just pass)

users :: [(Text, Text)]
users = [
    ("user@example.com", "password")
  ]

insertFixtures :: MonadIO m => ReaderT SqlBackend m ()
insertFixtures = do
  mapM_ insertUser users

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    insertFixtures
