{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shared
import Shelly (shellyNoDir, cd)
import Filesystem.Path.CurrentOS (fromText)

main :: IO ()
main = do
    args <- getArgs
    (root, dest) <-
        case args of
            [x, y] -> return (fromText x, fromText y)
            _ -> error "Usage: make-template <repo location> <destination file>"
    shellyNoDir $ do
        cd root
        createHsFiles "HEAD" dest
