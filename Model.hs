module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
