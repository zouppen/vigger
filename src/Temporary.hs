-- |Helpers for handling with temporary directories.
module Temporary where

import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Temp (withSystemTempDirectory)

-- |Forks thread and gives a temporary directory for it. Please note
-- that the temporary directory is volatile so if the thread dies it
-- takes the directory and its contents away.
forkWithTempDir :: (FilePath -> IO ()) -> IO (ThreadId, FilePath)
forkWithTempDir act = do
  var <- newEmptyTMVarIO
  tid <- forkIO $ withSystemTempDirectory "vigger" $ \tmpDir -> do
    atomically $ putTMVar var tmpDir
    act tmpDir
  dir <- atomically $ takeTMVar var
  pure (tid, dir)
