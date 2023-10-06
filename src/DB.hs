{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB (DB, runDB) where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Persist.Postgresql
  ( ConnectionString,
    SqlBackend,
    withPostgresqlConn,
  )
import Database.Persist.Sql qualified as SQL
import RIO (ByteString, ReaderT, Word16)

type DB a =
  ReaderT SqlBackend (LoggingT IO) a

data Config = Config
  { dbHost :: ByteString,
    dbPort :: Word16,
    dbName :: ByteString,
    dbUser :: ByteString,
    dbPass :: ByteString
  }

config :: Config
config =
  Config
    { dbHost = "localhost",
      dbPort = 5432,
      dbName = "toy-project",
      dbUser = "toy-project",
      dbPass = "toy-project"
    }

runDB :: DB a -> IO a
runDB action = do
  runStdoutLoggingT $
    withPostgresqlConn
      (toConnectionString config)
      (SQL.runSqlConn action)

toConnectionString :: Config -> ConnectionString
toConnectionString config' =
  Text.encodeUtf8
    ( Text.pack
        [i|host=#{dbHost config'} port=#{dbPort config'} dbname=#{dbName config'} user=#{dbUser config'} password=#{dbPass config'}|]
    )
