{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , runDB
    , Specs
    ) where

import Yesod.Test
import Database.Persist.MongoDB hiding (master)

type Specs = SpecsConn Connection

runDB :: Action IO a -> OneSpec Connection a
runDB = runDBRunner runMongoDBPoolDef
