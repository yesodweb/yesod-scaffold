module Handler.Comment where

import Import
import Data.Aeson
import Data.Aeson.TH

-- This should all go in Yesod proper
newtype Escaped = Escaped { getEscaped :: Text }
instance ToJSON Escaped where
  toJSON = toJSON . getEscaped
instance FromJSON Escaped where
  parseJSON = withText "Escaped" (return . Escaped . escapify)
-- TODO: where is an escaping function defined?
-- it is easy enough to write by hand
escapify = id


data CommentInput = CommentInput { message :: Escaped }
deriveFromJSON defaultOptions ''CommentInput

postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    commentInput <- requireJsonBody

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let comment = Comment { commentUserId = maybeCurrentUserId
                          , commentMessage = getEscaped $ message commentInput
                          }

    insertedComment <- runDB $ insertEntity comment
    returnJson insertedComment
