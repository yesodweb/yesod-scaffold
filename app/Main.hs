{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import RIO
import RIO.Orphans
import Conduit
import RIO.Process
import Options.Applicative.Simple
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (replace)
import RIO.Directory
import RIO.FilePath
import qualified Paths_yesod_scaffold
import Text.ProjectTemplate

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appResourceMap :: !ResourceMap
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasResourceMap App where
  resourceMapL = lens appResourceMap (\x y -> x { appResourceMap = y })

main :: IO ()
main = do
  (verbose, cmd) <- simpleOptions
    $(simpleVersion Paths_yesod_scaffold.version)
    "yesod-scaffold dev tools"
    "Provides functionality for working with the Yesod scaffolding"
    (switch (long "verbose" <> help "Enable verbose output")) $ do
      addCommand
        "build"
        "Compile all of the scaffoldings"
        build
        (switch (long "no-run-tests" <> help "Don't run the test suites"))
      addCommand
        "make-template"
        "Make a template"
        id
        (createHsFiles
          <$> strArgument (metavar "relative/path/to/scaffold")
          <*> strArgument (metavar "out.hsfiles")
          <*> (T.pack <$> strArgument (metavar "BRANCH" <> value "HEAD")))
      addCommand
        "merge"
        "Perform merge"
        merge
        (switch (long "push" <> help "Push to origin after merge"))
      addCommand
        "push"
        "Push to origin"
        id
        (pure push)
      addCommand
        "pull"
        "Pull from origin"
        id
        (pure pull)

  lo <- logOptionsHandle stdout verbose
  appProcessContext <- mkDefaultProcessContext
  withLogFunc lo $ \appLogFunc -> withResourceMap $ \appResourceMap ->
    runRIO App {..} cmd

build :: Bool -> RIO App ()
build noRunTests = do
    let testArgs
            | noRunTests = ["test", "--pedantic", "--no-run-tests"]
            | otherwise = ["test", "--pedantic"]
    void $ tryIO $ removeDirectoryRecursive "yesod-scaffold"
    proc "git" ["clone", ".", "yesod-scaffold"] runProcess_
    let run_ cmd args = withWorkingDir "yesod-scaffold" $ proc cmd args runProcess_
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", T.unpack branch]
        unless (branch == master) $ run_ "git" ["merge", T.unpack master]
        run_ "git" ["diff", "--exit-code"]
        run_ "stack" ["init"]
        run_ "stack" testArgs
        run_ "packdeps" ["PROJECTNAME.cabal"]
        run_ "git" ["clean", "-fxd"]
        createHsFiles "yesod-scaffold" ("hsfiles" </> T.unpack ("yesod-" <> branch) <.> "hsfiles") branch

merge :: Bool -> RIO App ()
merge push' = do
    forM_ branches $ \branch -> do
        proc "git" ["checkout", T.unpack branch] runProcess_
        unless (branch == master) $ proc "git" ["merge", T.unpack master] runProcess_
        proc "git" ["diff", "--exit-code"] runProcess_
        when push' $ proc "git" ["push", "-u", "origin", T.unpack branch] runProcess_
    proc "git" ["checkout", "master"] runProcess_

push :: RIO App ()
push = forM_ branches $ \branch -> proc "git" ["push", "origin", T.unpack branch] runProcess_

pull :: RIO App ()
pull = do
  proc "git" ["fetch", "origin"] runProcess_
  forM_ branches $ \branch -> do
    proc "git" ["checkout", T.unpack branch] runProcess_
    proc "git" ["merge", "origin/" ++ T.unpack branch] runProcess_
  proc "git" ["checkout", "master"] runProcess_

branches :: [Text]
branches = [ "postgres", "sqlite", "mysql", "mongo", "simple" -- , "postgres-fay"
           , "minimal"
           ]

master :: Text
master = "postgres"

-- | Works in the current Shelly directory. Confusingly, the @FilePath@
-- destination is relative to the original working directory.
createHsFiles :: FilePath -- ^ root
              -> FilePath -- ^ destination
              -> Text -- ^ branch
              -> RIO App ()
createHsFiles root fp branch = do
    (files, _err) <- withWorkingDir root
                   $ proc "git" ["ls-tree", "-r", T.unpack branch, "--name-only"]
                     readProcess_
    createDirectoryIfMissing True $ takeDirectory fp
    runConduit
        $ sourceLazy files
       .| decodeUtf8C
       .| linesUnboundedC
       .| mapC (toPair . T.unpack)
       .| filterC (not . isTravis)
       .| mapC (renameTravis)
       .| createTemplate
       .| mapMC replaceProjectName
       .| sinkFile fp
  where
    toPair :: FilePath -> (FilePath, RIO App ByteString)
    toPair fp' = (fp', readFileBinary $ root </> fp')

    -- We don't want to include the .travis.yml files, since these are specific
    -- to the yesod-scaffold repo somewhat
    isTravis (".travis.yml", _) = True
    isTravis _ = False

    -- Rename the __.travis.yml file intended for the user's project to .travis.yml
    renameTravis :: (FilePath, RIO App ByteString) -> (FilePath, RIO App ByteString)
    renameTravis ("__.travis.yml", x) = (".travis.yml", x)
    renameTravis a = a

    -- Replace the PROJECTNAME and PROJECTNAME_LOWER syntax for something Stack
    -- supports
    replaceProjectName bs =
      case decodeUtf8' bs of
        Left e -> throwM e
        Right text ->
          return $ encodeUtf8
                 $ T.replace "PROJECTNAME" "{{name}}"
                 $ T.replace "PROJECTNAME_LOWER" "{{name}}"
                 $ T.replace "PROJECTNAME_MODULE" "{{name-as-module}}"
                   text
