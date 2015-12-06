{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Shared where

import ClassyPrelude.Conduit
import Shelly (Sh, run)
import Text.ProjectTemplate (createTemplate)
import System.Directory
import System.FilePath
import qualified Data.Text as T

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
       $$ filterC (not . isTravis)
       =$ createTemplate
       =$ mapC replaceProjectName
       =$ sinkFile fp
  where
    toPair :: FilePath -> (FilePath, ResourceT IO ByteString)
    toPair fp' = (fp', readFile $ root </> fp')

    -- We don't want to include the .travis.yml files, since these are specific
    -- to the yesod-scaffold repo somewhat
    isTravis (".travis.yml", _) = True
    isTravis _ = False

    -- Replace the PROJECTNAME and PROJECTNAME_LOWER syntax for something Stack
    -- supports
    replaceProjectName = encodeUtf8
                       . T.replace "PROJECTNAME_LOWER" "{{name}}"
                       . T.replace "PROJECTNAME" "{{name}}"
                       . decodeUtf8
