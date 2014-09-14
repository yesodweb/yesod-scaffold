module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Data.Typeable (Typeable)
import Prelude

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoContext))
 in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models")
