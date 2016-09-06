module Handler.Home where

import Import
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderSematnicUiDivs $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")



-- @todo: Move to own yesod-sematnic-ui package?

-- Adaptation of renderDivs.
renderSematnicUiDivs :: Monad m => FormRender m a
renderSematnicUiDivs = renderSematnicUiDivsMaybeLabels True

-- Only difference here is that we add a ".field" class on the wrapper div.
renderSematnicUiDivsMaybeLabels :: Monad m => Bool -> FormRender m a
renderSematnicUiDivsMaybeLabels withLabels aform fragment = do
  (res, views') <- aFormToForm aform
  let views = views' []
  let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
  <div.field :fvRequired view:.required :not $ fvRequired view:.optional>
      $if withLabels
              <label for=#{fvId view}>#{fvLabel view}
      $maybe tt <- fvTooltip view
          <div .tooltip>#{tt}
      ^{fvInput view}
      $maybe err <- fvErrors view
          <div .errors>#{err}
|]
  return (res, widget)
