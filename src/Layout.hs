module Layout where

import Import.NoFoundation
import Foundation

-- This gets re-exported in Import because basically every
-- handler will need this. Feel free to add alternative 
-- layout functions or widget utility functions.

layout :: Widget -> Handler Html
layout widget = do
  master <- getYesod
  mmsg <- getMessage
  pc <- widgetToPageContent $ do
    addBootstrap
    $(widgetFile "layout")
  defaultLayoutWrapper pc

