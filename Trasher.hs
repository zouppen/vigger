module Trasher where

import Data.ByteString (ByteString)
import System.Posix.Files.ByteString (removeLink)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (forever, guard)

type Trash = ByteString -> STM ()

-- |Create queue which just removes everything sent there 
runTrasher :: IO (ThreadId, Trash)
runTrasher = do
  q <- newTQueueIO
  tid <- forkIO $ forever $ do
    list <- atomically $ do
      isEmptyTQueue q >>= guard . not
      flushTQueue q
    mapM_ removeLink list
  pure (tid, writeTQueue q)

-- |Run trasher and kill thread when the action is finished
withTrasher :: (Trash -> IO ()) -> IO ()
withTrasher act = do
  (tid, putter) <- runTrasher
  act putter
  killThread tid

