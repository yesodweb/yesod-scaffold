{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
<<<<<<< HEAD
=======
    , module Model
    , module Database.Persist
    , runDB
>>>>>>> postgres
    , Specs
    ) where

import Yesod.Test

type Specs = SpecsConn ()
