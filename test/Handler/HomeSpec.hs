module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
          get HomeR
          statusIs 200
          htmlAllContain "h1" "Welcome to Yesod"

        statusIs 200
        -- more debugging printBody
        htmlCount ".message" 1
        htmlAllContain ".message" "Some Content"
        htmlAllContain ".message" "text/plain"
