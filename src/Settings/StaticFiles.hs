{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
staticFiles (appStaticDir compileTimeAppSettings)

-- If you prefer to updating the references by force
--  -- especially when you are devloping like `stack exec -- yesod devel` --
-- you can update references by chaning file stamp.
--
-- On linux or Unix-like system, you can use
--     shell> touch /Path/To/Settings/StaticFiles.hs
--
-- or save without changes on your favorite editor.
-- so yesod devel will re-compile automatically and generate the refereces
-- including new one.
--
-- In this way you can use on your shakespearean template(s)
-- Let's say you have image on "yourStaticDir/img/background-1.jpg"
-- you can use the reference of the file on shakespearean templates like bellow
--
--     /* note: `-' becomes '_' as well */
--     @{StaticR img_background_1_jpg}
