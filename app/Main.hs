{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import ClassyPrelude.Conduit
import System.Process.Typed
import Options.Applicative.Simple
import qualified Data.Text as T
import System.Directory
import System.FilePath
import qualified Paths_yesod_scaffold
import Text.ProjectTemplate

main :: IO ()
main = do
  ((), cmd) <- simpleOptions
    $(simpleVersion Paths_yesod_scaffold.version)
    "yesod-scaffold dev tools"
    "Provides functionality for working with the Yesod scaffolding"
    (pure ()) $ do
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
          <$> strArgument (metavar "REPO")
          <*> strArgument (metavar "DEST")
          <*> (pack <$> strArgument (metavar "BRANCH" <> value "HEAD")))
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
  cmd

build :: Bool -> IO ()
build noRunTests = do
    let testArgs
            | noRunTests = ["test", "--pedantic", "--no-run-tests"]
            | otherwise = ["test", "--pedantic"]
    void $ tryIO $ removeDirectoryRecursive "yesod-scaffold"
    runProcess_ $ proc "git" ["clone", ".", "yesod-scaffold"]
    let run_ cmd args = runProcess_ $ setWorkingDir "yesod-scaffold" $ proc cmd args
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", unpack branch]
        unless (branch == master) $ run_ "git" ["merge", unpack master]
        run_ "git" ["diff", "--exit-code"]
        run_ "stack" ["init"]
        run_ "stack" testArgs
        run_ "packdeps" ["PROJECTNAME.cabal"]
        run_ "git" ["clean", "-fxd"]
        createHsFiles "yesod-scaffold" ("hsfiles" </> unpack ("yesod-" ++ branch) <.> "hsfiles") branch

merge :: Bool -> IO ()
merge push' = do
    forM_ branches $ \branch -> do
        runProcess_ $ proc "git" ["checkout", unpack branch]
        unless (branch == master) $ runProcess_ $ proc "git" ["merge", unpack master]
        runProcess_ $ proc "git" ["diff", "--exit-code"]
        when push' $ runProcess_ $ proc "git" ["push", "-u", "origin", unpack branch]
    runProcess_ $ proc "git" ["checkout", "master"]

push :: IO ()
push = forM_ branches $ \branch -> runProcess_ $ proc "git" ["push", "origin", unpack branch]

branches :: [Text]
branches = [ "postgres", "sqlite", "mysql", "mongo", "simple", "postgres-fay"
           , "minimal"
           ]

master :: Text
master = "postgres"

-- | Works in the current Shelly directory. Confusingly, the @FilePath@
-- destination is relative to the original working directory.
createHsFiles :: FilePath -- ^ root
              -> FilePath -- ^ destination
              -> Text -- ^ branch
              -> IO ()
createHsFiles root fp branch = do
    (files, _err) <- readProcess_
                   $ setWorkingDir root
                   $ proc "git" ["ls-tree", "-r", unpack branch, "--name-only"]
    createDirectoryIfMissing True $ takeDirectory fp
    runConduitRes
        $ mapM_ (yield . toPair. unpack) (lines (decodeUtf8 files))
       .| filterC (not . isTravis)
       .| mapC (renameTravis)
       .| createTemplate
       .| mapC replaceProjectName
       .| sinkFile fp
  where
    toPair :: FilePath -> (FilePath, ResourceT IO ByteString)
    toPair fp' = (fp', readFile $ root </> fp')

    -- We don't want to include the .travis.yml files, since these are specific
    -- to the yesod-scaffold repo somewhat
    isTravis (".travis.yml", _) = True
    isTravis _ = False

    -- Rename the __.travis.yml file intended for the user's project to .travis.yml
    renameTravis :: (FilePath, ResourceT IO ByteString) -> (FilePath, ResourceT IO ByteString)
    renameTravis ("__.travis.yml", x) = (".travis.yml", x)
    renameTravis a = a

    -- Replace the PROJECTNAME and PROJECTNAME_LOWER syntax for something Stack
    -- supports
    replaceProjectName = encodeUtf8
                       . T.replace "PROJECTNAME_LOWER" "{{name}}"
                       . T.replace "PROJECTNAME" "{{name}}"
                       . decodeUtf8
