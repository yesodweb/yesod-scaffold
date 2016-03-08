{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shelly (shelly, run_)
import Shared

main :: IO ()
main = shelly $ do
    args <- getArgs
    forM_ branches $ \branch -> do
        run_ "git" ["checkout", branch]
        unless (branch == master) $ run_ "git" ["merge", master]
        run_ "git" ["diff", "--exit-code"]
        when ("--push" `elem` args) $ run_ "git" ["push", "-u", "origin", branch]
    run_ "git" ["checkout", "master"]
