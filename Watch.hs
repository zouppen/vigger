{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Watch ( Watch(stopWatch, lastEnqueue, workDir)
             , startCapture
             , stopCapture
             , forkWatch
             , FileEvent(..)
             , RawFilePath -- re-export
             ) where

import System.INotify
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Foreign.C.Types (CTime)
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.STM
import Control.Monad (when, forever, guard)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import System.FilePath.ByteString

import Exceptions
import Trasher (Trash)
import Temporary

data FileEvent = FileEvent { time :: EpochTime
                           , path :: RawFilePath
                           } deriving (Show)

data Watch = Watch
  { eventQueue   :: TQueue FileEvent       -- ^Queue holding events ("ring buffer")
  , eventInHand  :: TVar (Maybe FileEvent) -- ^Currently processed event
  , purgeEnabled :: TVar Bool              -- ^Is file removing active
  , lastEnqueue  :: IO FileEvent           -- ^Action returning last
                                           -- incoming event time.
  , trash        :: Trash                  -- ^File remover
  , stopWatch    :: IO ()                  -- ^Stop watch. Doesn't
                                           -- remove files. Do not
                                           -- call twice.
  , workDir      :: FilePath               -- ^Temporary directory holding the videos
  }

-- |Start capture
startCapture :: Watch -> STM ()
startCapture Watch{..} = do
  -- Remove the item in hand if it is
  readTVar eventInHand >>= maybe nop (unGetTQueue eventQueue)
  -- Stop the purge
  writeTVar purgeEnabled False

-- |Stop capture, returns file names
stopCapture :: Watch -> STM [RawFilePath]
stopCapture watch@Watch{..} = do
  purging <- readTVar purgeEnabled
  when purging $ throwSTM $ ViggerNonFatal "motionStop called without motionStart"
  -- Everything is fine, let's starting the purge again
  writeTVar purgeEnabled True
  -- Empty the event queue to a list
  map path <$> flushTQueue eventQueue

-- |Creates new temporary directory and starts watching closed files
-- in that directory and purging them after given timeout.
forkWatch :: Trash -> CTime -> INotify -> IO Watch
forkWatch trash maxAge ih = do
  eventQueue <- newTQueueIO
  eventInHand <- newTVarIO Nothing
  purgeEnabled <- newTVarIO True -- Motion stopped at start
  -- Initialize dead man switch
  lastEnqueueVar <- newTVarIO $ FileEvent 0 ""
  let lastEnqueue = readTVarIO lastEnqueueVar
  (purgeThread, workDir) <- forkWithTempDir $ \workDir -> forever $
    purgeEvent maxAge Watch{stopWatch = undefined, ..}
  -- INotify uses RawFilePath internally so we need to convert between
  -- FilePath and RawFilePath here.
  watchDesc <- watchClosures ih (encodeFilePath workDir) $ \path -> do
    time <- epochTime
    atomically $ do
      let e = FileEvent{..}
      writeTQueue eventQueue e
      writeTVar lastEnqueueVar e
  let stopWatch = do
        removeWatch watchDesc
        killThread purgeThread
  pure Watch{..}

-- |Watch given directory, enqueuing closed files as they occur.
watchClosures :: INotify -> RawFilePath -> (RawFilePath -> IO ()) -> IO WatchDescriptor
watchClosures h dir send = addWatch h [CloseWrite] dir handleEvent
  where handleEvent = maybe nop action . toClosedFile
        action file = send $ dir </> file

-- |Converts Event to file name of the closed file or Nothing if it's
-- something else.
toClosedFile :: Event -> Maybe RawFilePath
toClosedFile Closed{..} = if isDirectory then Nothing else maybeFilePath
toClosedFile _ = Nothing

-- |Purge old file if we have permission to do so. Has a problem in
-- where an event might be lost if startMotion and stopMotion occur
-- between two transactions.
purgeEvent :: CTime -> Watch -> IO ()
purgeEvent maxAge Watch{..} = do
  FileEvent{..} <- atomically $ do
    -- Keep going only if purging is enabled
    readTVar purgeEnabled >>= guard
    -- Take one item and place it to "hand" to avoid race conditions.
    a <- readTQueue eventQueue
    writeTVar eventInHand $ Just a
    pure a

  -- Sleep until it should expire
  do
    now <- epochTime
    let expiresIn = time - now + maxAge
    when (expiresIn > 0) $ threadDelay $ 1000000 * fromEnum expiresIn

  -- If we have the event still in hand, it should be
  -- removed. Otherwise fallthrough.
  atomically $ do
    enabled <- isJust <$> readTVar eventInHand
    when enabled $ do
      trash path
      writeTVar eventInHand Nothing

-- |Shorthand for doing nothing
nop :: Applicative m => m ()
nop = pure ()
