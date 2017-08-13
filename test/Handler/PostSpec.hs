{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PostSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Post payload" $ do
        it "asserts no access to my-account for anonymous users" $ do
            get PostsR
            statusIs 200
