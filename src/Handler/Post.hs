module Handler.Post where

import Import

getPostR :: PostId -> Handler Value
getPostR pid = do
    post <- runDB $ get404 pid

    return $ object ["post" .= (Entity pid post)]

putPostR :: PostId -> Handler Value
putPostR pid = do
    post <- requireJsonBody :: Handler Post

    runDB $ replace pid post

    sendResponseStatus status200 ("UPDATED" :: Text)

deletePostR :: PostId -> Handler Value
deletePostR pid = do
    runDB $ delete pid

    sendResponseStatus status200 ("DELETED" :: Text)
