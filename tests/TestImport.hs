{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , runDB
    , Specs
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (runSqlPool, SqlPersist, Connection)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.IO.Class (liftIO)

import Foundation
import Model

type Specs = YesodSpec App

runDB :: SqlPersist (NoLoggingT (ResourceT IO)) a -> YesodExample App a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runResourceT $ runNoLoggingT $ runSqlPool query pool
