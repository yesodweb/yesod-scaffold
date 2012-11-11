{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Shared where

import ClassyPrelude.Conduit
import Shelly (Sh, run, fromText)
import Text.ProjectTemplate (createTemplate)
import Filesystem (createTree)
import Filesystem.Path (directory)

branches :: [LText]
branches = ["postgres", "sqlite", "mysql", "mongo", "simple"]

master :: LText
master = "postgres"

-- | Works in the current Shelly directory. Confusingly, the @FilePath@
-- destination is relative to the original working directory.
createHsFiles :: LText -- ^ branch
              -> FilePath -- ^ destination
              -> Sh ()
createHsFiles branch fp = do
    files <- run "git" ["ls-tree", "-r", branch, "--name-only"]
    liftIO $ createTree $ directory fp
    liftIO
        $ runResourceT
        $ mapM_ (yield . toPair "yesod-scaffold" . fromText) (lines files)
       $$ createTemplate
       =$ writeFile fp
  where
    toPair root fp' = (fp', readFile $ root </> fp')
