module Web.Handler.Comment where

import DB (runDB)
import DB.JSON (Comment (commentUserId))
import Web.Import
  ( Handler,
    MonadIO (liftIO),
    Value,
    YesodAuth (maybeAuthId),
    insertEntity,
    requireCheckJsonBody,
    returnJson,
  )

postCommentR :: Handler Value
postCommentR = do
  -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
  -- (The ToJSON and FromJSON instances are derived in the config/models file).
  comment <- (requireCheckJsonBody :: Handler Comment)

  -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
  maybeCurrentUserId <- maybeAuthId
  let comment' = comment {commentUserId = maybeCurrentUserId}

  insertedComment <- liftIO $ runDB $ insertEntity comment'
  returnJson insertedComment
