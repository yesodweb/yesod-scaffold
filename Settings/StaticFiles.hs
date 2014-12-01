module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles, StaticRoute)

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
staticFiles (appStaticDir compileTimeAppSettings)

defaultCss :: [StaticRoute]
defaultCss = [ css_bootstrap_css
             ]
