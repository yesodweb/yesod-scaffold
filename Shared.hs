{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Shared where

import ClassyPrelude.Conduit

branches :: [LText]
branches = ["postgres", "sqlite", "mysql", "mongo", "simple"]

master :: LText
master = "postgres"
