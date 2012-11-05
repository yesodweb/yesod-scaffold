{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , runDB
    , Specs
    ) where

import Yesod.Test
import Database.Persist.GenericSql

type Specs = SpecsConn Connection

runDB :: SqlPersist IO a -> OneSpec Connection a
runDB = runDBRunner runSqlPool
