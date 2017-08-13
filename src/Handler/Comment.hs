module Handler.Comment where

import Import

getCommentR :: PostId -> CommentId -> Handler Value
getCommentR _ cid = do
    comment <- runDB $ get404 cid

    return $ object ["comment" .= (Entity cid comment)]

deleteCommentR :: PostId -> CommentId -> Handler ()
deleteCommentR _ cid = do
    runDB $ delete cid

    sendResponseStatus status200 ("DELETED" :: Text)
