{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Shared where

import ClassyPrelude.Conduit
import Shelly (Sh, run, fromText)
import Text.ProjectTemplate (createTemplate)
import Filesystem (createTree)
import Filesystem.Path (directory)
import Data.Conduit.Filesystem (sinkFile)

branches :: [LText]
branches = ["postgres", "sqlite", "mysql", "mongo", "simple", "postgres-fay"]

master :: LText
master = "postgres"

-- | Works in the current Shelly directory. Confusingly, the @FilePath@
-- destination is relative to the original working directory.
createHsFiles :: FilePath -- ^ root
              -> LText -- ^ branch
              -> FilePath -- ^ destination
              -> Sh ()
createHsFiles root branch fp = do
    files <- run "git" ["ls-tree", "-r", branch, "--name-only"]
    liftIO $ createTree $ directory fp
    liftIO
        $ runResourceT
        $ mapM_ (yield . toPair . fromText) (lines files)
       $$ createTemplate
       =$ sinkFile fp
  where
    toPair fp' = (fp', readFile $ root </> fp')
