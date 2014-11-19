module Main where

import Import
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)
import Yesod.Default.Config2

import qualified Handler.HomeSpec

main :: IO ()
main = do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        IgnoreEnv
    foundation <- makeFoundation settings
    hspec $ do
        yesodSpec foundation $ do
            Handler.HomeSpec.spec
