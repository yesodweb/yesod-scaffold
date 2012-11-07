{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shelly (shellyNoDir, rm_rf, run_, run, fromText, cd)
import Data.Conduit.Filesystem (sinkFile)
import Text.ProjectTemplate (createTemplate)
import Filesystem (createTree)
import Filesystem.Path (directory)
import Shared

main :: IO ()
main = shellyNoDir $ do
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", branch]
        unless (branch == master) $ run_ "git" ["merge", master]
        run_ "git" ["diff", "--exit-code"]
    run_ "git" ["checkout", "master"]
