module App.Route.Home.Form where

import App.NoFoundationImport

import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3
    , withSmallInput )


sampleForm :: Form app (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
