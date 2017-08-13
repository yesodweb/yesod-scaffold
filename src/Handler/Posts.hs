module Handler.Posts where

import Import

getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [] [] :: Handler [Entity Post]

    return $ object ["posts" .= posts]

postPostsR :: Handler ()
postPostsR = do
    post <- requireJsonBody :: Handler Post
    _    <- runDB $ insert post

    sendResponseStatus status201 ("CREATED" :: Text)

