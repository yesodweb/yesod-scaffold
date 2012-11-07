{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Language.Fay.Prelude
import Language.Fay.Yesod
import Language.Fay.FFI

data Command = GetFib Int (Returns Int)
    deriving (Read, Typeable, Data)
instance Foreign Command
