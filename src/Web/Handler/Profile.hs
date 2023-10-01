{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Handler.Profile where

import DB.User (User (userIdent))
import Web.Import
  ( Handler,
    Html,
    Semigroup ((<>)),
    Yesod (defaultLayout),
    requireAuthPair,
    setTitle,
    toHtml,
    widgetFile,
    ($),
    (.),
  )

getProfileR :: Handler Html
getProfileR = do
  (_, user) <- requireAuthPair
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "profile")
