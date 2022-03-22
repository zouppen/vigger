module Trasher where

import System.FilePath.ByteString (RawFilePath)
import System.Posix.Files.ByteString (removeLink)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (forever, guard)
import Data.Foldable (traverse_)

type Trash = RawFilePath -> STM ()

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

-- |Send list of files to trasher
trashIO :: Foldable t => Trash -> t RawFilePath -> IO ()
trashIO trash = atomically . traverse_ trash

-- |Run trasher and kill thread when the action is finished
withTrasher :: (Trash -> IO ()) -> IO ()
withTrasher act = do
  (tid, putter) <- runTrasher
  act putter
  killThread tid

