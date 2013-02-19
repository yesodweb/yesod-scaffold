{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Database.Persist
    , runDB
    , Specs
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.MongoDB hiding (master)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)

import Model

type Specs = SpecsConn Connection

runDB :: Action (NoLoggingT (ResourceT IO)) a -> OneSpec Connection a
runDB = runDBRunner poolRunner
  where
    poolRunner query pool = runResourceT
                          $ runNoLoggingT
                          $ runMongoDBPoolDef query pool
