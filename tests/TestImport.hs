{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Foundation
    , Spec
    , Example
    ) where

import Yesod.Test
import Foundation

type Spec = YesodSpec App
type Example = YesodExample App
