module Task.Cron 
  ( scheduleAllJobs
  ) where

import Import
import System.Cron.Schedule
import qualified Task as Task
import qualified Data.Text as Text
import Control.Monad.Catch (catchAll)

scheduleAllJobs :: App -> IO ()
scheduleAllJobs app = do
  _ <- execSchedule $ do
    addJob (runAndLogErrors app heartbeat ) "* * * * *"
  runReaderT (Task.log "All cronjobs have been scheduled") app
  
heartbeat :: Task ()
heartbeat = Task.log "Repeated jobs heartbeat"

runAndLogErrors :: App -> Task () -> IO ()
runAndLogErrors app task = flip runReaderT app
  $ catchAll task (\e -> Task.log ("EXCEPTION: " <> Text.pack (show e)))

