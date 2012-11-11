{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shelly (shellyNoDir, rm_rf, run_, fromText, cd)
import Shared

main :: IO ()
main = shellyNoDir $ do
    rm_rf "yesod-scaffold"
    run_ "git" ["clone", ".", "yesod-scaffold"]
    cd "yesod-scaffold"
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", branch]
        unless (branch == master) $ run_ "git" ["merge", master]
        run_ "git" ["diff", "--exit-code"]
        run_ "cabal" ["install", "--only-dependencies"]
        run_ "yesod" ["test"]
        run_ "git" ["clean", "-fxd"]
        createHsFiles "yesod-scaffold" branch $ "hsfiles" </> fromText branch <.> "hsfiles"
