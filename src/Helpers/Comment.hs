module Helpers.Comment where

import Import

-- This datatype would be richer if Comment had more attributes. For now
-- we only have to deal with content, so I can use a simple newtype.
newtype CommentAttrs = CommentAttrs Text

instance FromJSON CommentAttrs where
    parseJSON (Object o) = CommentAttrs <$> o .: "content"
    parseJSON _          = mzero

toComment :: PostId -> CommentAttrs -> Comment
toComment pid (CommentAttrs content) = Comment
    { commentPost    = pid
    , commentContent = content
    }
