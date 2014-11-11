module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , module Prelude
    , runDB
    , Spec
    , Example
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Prelude

import Foundation
import Model

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool
