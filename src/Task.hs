module Task 
  ( db
  , log
  , mngr
  , render
  , renderer
  , gets
  ) where

-- This module should be imported qualified
import Import hiding (log, get) 
import System.Log.FastLogger
import Database.Persist.Sql hiding (get)
import Yesod.Core.Types
import Network.HTTP.Client (Manager)

db :: SqlPersistT IO a -> Task a
db s = do
  p <- asks appConnPool
  lift $ runSqlPool s p

log :: Text -> Task ()
log t = do
  l <- asks appLogger
  lift $ pushLogStrLnTimestamp l t

mngr :: Task Manager
mngr = asks appHttpManager

render :: Route App -> Task Text
render r = fmap ($ r) renderer

renderer :: Task (Route App -> Text)
renderer = do
  app <- ask 
  return $ \r -> yesodRender app (resolveApproot app) r []

gets :: (App -> a) -> Task a
gets f = fmap f ask 

pushLogStrLnTimestamp :: Logger -> Text -> IO ()
pushLogStrLnTimestamp (Logger set dateCacheGetter) logStr = do
  bsNow <- dateCacheGetter
  pushLogStrLn set (toLogStr bsNow <> " " <> toLogStr logStr)

-- Copied because yesod-core does not export this function
resolveApproot :: Yesod master => master -> ResolvedApproot
resolveApproot master = case approot of
  ApprootRelative  -> ""
  ApprootStatic t  -> t
  ApprootMaster f  -> f master
  ApprootRequest f -> f master (error "resolveApproot failed. You cant use ApprootRequest")

