{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

<<<<<<< HEAD
import Add
import Home
=======
-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Comment
import Handler.Profile
>>>>>>> postgres

mkYesodDispatch "App" resourcesApp
