{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , runDB
    , Spec
    , Example
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.MongoDB hiding (master)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.IO.Class (liftIO)

import Foundation
import Model

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: Action (NoLoggingT (ResourceT IO)) a -> YesodExample App a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runResourceT $ runNoLoggingT $ runMongoDBPoolDef query pool
