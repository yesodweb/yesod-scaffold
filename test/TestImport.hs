module TestImport
    ( module Yesod.Test
    , module Foundation
    , module Prelude
    , Spec
    , Example
    ) where

import Yesod.Test
import Prelude
import Foundation

type Spec = YesodSpec App
type Example = YesodExample App
