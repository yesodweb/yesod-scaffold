{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-missing-signatures #-}

-- | This module exists solely to disable warnings.
module Import.NoWarning (runFakeHandler) where
import qualified Yesod.Core
runFakeHandler = Yesod.Core.runFakeHandler
