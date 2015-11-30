{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shelly (shelly, rm_rf, run_, cd)
import Shared

main :: IO ()
main = shelly $ do
    rm_rf "yesod-scaffold"
    run_ "git" ["clone", ".", "yesod-scaffold"]
    cd "yesod-scaffold"
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", branch]
        unless (branch == master) $ run_ "git" ["merge", master]
        run_ "git" ["diff", "--exit-code"]
        run_ "cabal" ["install", "--only-dependencies", "--force-reinstalls", "--enable-tests", "--reorder-goals", "--max-backjumps=-1"]
        run_ "yesod" ["test"]
        run_ "git" ["clean", "-fxd"]
        createHsFiles "yesod-scaffold" branch $ "hsfiles" </> unpack branch <.> "hsfiles"
