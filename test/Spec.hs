module Main where

import Import
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation, loadAppSettings)

import qualified Handler.HomeSpec

main :: IO ()
main = do
    settings <- loadAppSettings True
    foundation <- makeFoundation settings
    hspec $ do
        yesodSpec foundation $ do
            Handler.HomeSpec.spec
