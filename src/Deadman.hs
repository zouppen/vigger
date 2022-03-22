-- |General dead man switch functions.
module Deadman where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when, forever)
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Data.Function (fix)

-- |Dead man switch. It takes timeout and an action to retrieve
-- last action time. This function returns when more than number of
-- timeout seconds have elapsed after last action time.
awaitDeadline :: EpochTime -> IO EpochTime -> IO ()
awaitDeadline timeout getLastActionTime = fix $ \loop -> do
  now <- epochTime
  last <- getLastActionTime
  let target = last + timeout - now
  when (target > 0) $ do
    threadDelay $ 1000000 * fromEnum target
    loop

-- |Dead man switch loop. It takes timeout, an action to retrieve last
-- action time and actions for starting and stopping the action. If
-- this thread is terminated, the child cleanup is performed, too.
deadManLoop :: EpochTime -> IO EpochTime -> IO a -> (a -> IO ()) -> IO ()
deadManLoop timeout getLastActionTime start clean = forever $ do
  -- Make sure the operation is stopped (cleaned) after the deadline
  bracket start clean $ const $ do
    -- Always wait one timeout to let it start properly
    threadDelay $ 1000000 * fromEnum timeout
    -- Wait until it stops working
    awaitDeadline timeout getLastActionTime
