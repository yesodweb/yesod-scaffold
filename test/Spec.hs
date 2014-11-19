module Main where

import Import
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation, loadAppSettings)

import qualified Handler.HomeSpec

main :: IO ()
main = do
    settings <- loadAppSettings
        ["config/settings-test.yml", "config/settings.yml"]
        []
        False
    foundation <- makeFoundation settings
    hspec $ do
        yesodSpec foundation $ do
            Handler.HomeSpec.spec
