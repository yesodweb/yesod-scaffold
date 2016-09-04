{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shelly (shelly, run_)
import Shared

main :: IO ()
main = shelly $ do
    args <- getArgs
    forM_ branches $ \branch -> run_ "git" ["push", "origin", branch]
