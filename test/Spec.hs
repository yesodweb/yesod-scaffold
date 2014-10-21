module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)

import qualified Handler.HomeSpec

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    hspec $ do
        yesodSpec foundation $ do
            Handler.HomeSpec.spec
