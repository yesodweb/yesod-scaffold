{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Shared where

import ClassyPrelude.Conduit
import Shelly (Sh, run)
import Text.ProjectTemplate (createTemplate)
import System.Directory
import System.FilePath

branches :: [Text]
branches = [ "postgres", "sqlite", "mysql", "mongo", "simple", "postgres-fay"
           , "minimal"
           ]

master :: Text
master = "postgres"

-- | Works in the current Shelly directory. Confusingly, the @FilePath@
-- destination is relative to the original working directory.
createHsFiles :: FilePath -- ^ root
              -> Text -- ^ branch
              -> FilePath -- ^ destination
              -> Sh ()
createHsFiles root branch fp = do
    files <- run "git" ["ls-tree", "-r", branch, "--name-only"]
    liftIO $ createDirectoryIfMissing True $ takeDirectory fp
    liftIO
        $ runResourceT
        $ mapM_ (yield . toPair. unpack) (lines (files :: Text))
       $$ createTemplate
       =$ sinkFile fp
  where
    toPair :: FilePath -> (FilePath, ResourceT IO ByteString)
    toPair fp' = (fp', readFile $ root </> fp')
