{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude.Conduit
import Shared
import Shelly (shelly, cd)

main :: IO ()
main = do
    args <- getArgs
    (root, dest) <-
        case args of
            [x, y] -> return (unpack x, unpack y)
            _ -> error "Usage: make-template <repo location> <destination file>"
    shelly $ do
        cd $ fromString root
        createHsFiles root "HEAD" dest
