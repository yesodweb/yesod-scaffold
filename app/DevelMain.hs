-- | Development version to be run inside GHCi.
--
-- start this up with:
--
-- cabal repl --ghc-options="-O0 -fobject-code"
--
-- You will need to add these packages to your .cabal file
-- * foreign-store (very light-weight)
-- * warp (you already depend on this, it just isn't in your .cabal file)
--
-- If you don't use cabal repl, you will need
-- to run the following in GHCi or to add it to
-- your .ghci file.
--
-- :set -DDEVELOPMENT
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import Application (getApplicationDev)

import Control.Exception (finally)
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp

-- | Start or restart the server.
update :: IO ()
update = do
    mtidStore <- lookupStore tid_1
    case mtidStore of
      Nothing -> do
          done <- newEmptyMVar
          _done_0 <- newStore done
          tid <- start done
          tidRef <- newIORef tid
          _tid_1 <- newStore tidRef
          return ()
      Just tidStore -> do
          tidRef <- readStore tidStore
          tid <- readIORef tidRef
          done <- readStore (Store done_0)
          killThread tid
          takeMVar done
          newTid <- start done
          writeIORef tidRef newTid
  where tid_1 = 1
        done_0 = 0

-- | Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = do
    (port,app) <- getApplicationDev
    forkIO (finally (runSettings (setPort port defaultSettings) app)
                    (putMVar done ()))
