{-# LANGUAGE NoImplicitPrelude #-}

import Import

import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn)
import Control.Monad.Logger (runStderrLoggingT)

insertUser :: MonadIO m => (Text, Text) -> ReaderT SqlBackend m ()
insertUser (ident, pass) = do
  insert_ $ User ident (Just pass)

insertPost :: MonadIO m => (Text, Text) -> ReaderT SqlBackend m ()
insertPost (title, content) = do
  -- every post gets a comment!
  p <- insert $ Post title content
  insert_ $ Comment p ("Cool, you said " <> content)

users :: [(Text, Text)]
users = [
    ("user@example.com", "password")
  ]

posts :: [(Text, Text)]
posts = [
    ("a post", "Lorem Ipsum, blah blah")
  , ("another post", "blub blub")
  ]

insertFixtures :: MonadIO m => ReaderT SqlBackend m ()
insertFixtures = do
  mapM_ insertUser users
  mapM_ insertPost posts

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    insertFixtures
