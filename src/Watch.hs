{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Watch ( Watch(stopWatch, lastEnqueue, workDir, watchCamera)
             , startCapture
             , stopCapture
             , forkWatch
             , FileEvent(..)
             , Capture(..)
             , RawFilePath -- re-export
             ) where

import System.INotify
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import System.Process
import Foreign.C.Types (CTime)
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.STM
import Control.Monad (when, forever, guard, void)
import Data.Foldable (traverse_)
import System.FilePath.ByteString
import Control.Applicative

import Exceptions
import Trasher (Trash)
import Temporary
import Config(Camera(..))

data FileEvent = FileEvent { time :: EpochTime
                           , path :: RawFilePath
                           } deriving (Show)

-- |Capture data type. Returns files and a cleanup action for them.
data Capture = Capture
  { captureFiles  :: [RawFilePath] -- ^Captured files
  , captureClean  :: STM ()        -- ^Action to remove files
  , captureCamera :: Camera        -- ^User data about the camera
  }

data Watch = Watch
  { eventQueue   :: TQueue FileEvent -- ^Queue holding events ("ring buffer")
  , eventInHand  :: TMVar FileEvent  -- ^Currently processed event
  , purgeEnabled :: TVar Bool        -- ^Is file removing active
  , lastEnqueue  :: STM FileEvent    -- ^Action returning last
                                     -- incoming event time.
  , trash        :: Trash            -- ^File remover
  , stopWatch    :: IO ()            -- ^Stop watch. Doesn't remove
                                     -- files. Do not call twice.
  , workDir      :: FilePath         -- ^Temporary directory holding the videos
  , watchCamera  :: Camera           -- ^User data
  }

-- |Start capture
startCapture :: Watch -> STM ()
startCapture Watch{..} = do
  -- Remove the item from hand if there's any
  noRetry $ takeTMVar eventInHand >>= unGetTQueue eventQueue
  -- Stop the purge
  writeTVar purgeEnabled False

-- |Stop capture, returns file names
stopCapture :: Watch -> STM Capture
stopCapture watch@Watch{..} = do
  purging <- readTVar purgeEnabled
  when purging $ throwSTM $ ViggerNonFatal "motionStop called without motionStart"
  -- Everything is fine, let's starting the purge again
  writeTVar purgeEnabled True
  -- Empty the event queue to a list
  captureFiles <- map path <$> flushTQueue eventQueue
  let captureClean = traverse_ trash captureFiles
      captureCamera = watchCamera
  pure Capture{..}

-- |Creates new temporary directory and starts watching closed files
-- in that directory and purging them after given timeout.
forkWatch :: Trash -> Camera -> INotify -> IO Watch
forkWatch trash watchCamera ih = do
  eventQueue <- newTQueueIO
  eventInHand <- newEmptyTMVarIO
  purgeEnabled <- newTVarIO True -- Motion stopped at start
  -- Initialize dead man switch
  lastEnqueueVar <- newTVarIO $ FileEvent 0 ""
  let lastEnqueue = readTVar lastEnqueueVar
  (purgeThread, workDir) <- forkWithTempDir $ \workDir -> forever $
    purgeEvent Watch{stopWatch = undefined, ..}
  -- INotify uses RawFilePath internally so we need to convert between
  -- FilePath and RawFilePath here.
  watchDesc <- watchClosures ih (encodeFilePath workDir) $ \path -> do
    time <- epochTime
    atomically $ do
      let e = FileEvent{..}
      writeTQueue eventQueue e
      writeTVar lastEnqueueVar e
    -- Optionally execute command after the file. NB! This just forks
    -- it without following the child, so the user must make sure the
    -- scripts finish in a reasonable time.
    maybe nop (void . forkScript path) (exec watchCamera)
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

-- |Purge old file if we have permission to do so.
purgeEvent :: Watch -> IO ()
purgeEvent Watch{..} = do
  FileEvent{..} <- atomically $ do
    -- Keep going only if purging is enabled
    readTVar purgeEnabled >>= guard
    -- Take one item and place it to "hand" to avoid race conditions.
    a <- readTQueue eventQueue
    putTMVar eventInHand a
    pure a

  -- Sleep until it should expire
  do
    now <- epochTime
    let maxAge = toEnum $ precapture watchCamera
        expiresIn = time - now + maxAge
    when (expiresIn > 0) $ threadDelay $ 1000000 * fromEnum expiresIn

  -- If we have the event still in hand, it should be
  -- removed. Otherwise we keep the file.
  atomically $ noRetry $ do
    takeTMVar eventInHand
    trash path

-- |Fork script into background
forkScript :: RawFilePath -> FilePath -> IO ProcessHandle
forkScript file cmd = do
  (_, _, _, ph) <- createProcess cp
  pure ph
  where cp = (proc cmd [decodeFilePath file]){ std_in = NoStream
                                             , close_fds = True
                                             }

-- |Shorthand for doing nothing
nop :: Applicative m => m ()
nop = pure ()

-- |No retry in case transaction retries.
noRetry :: Alternative f => f () -> f ()
noRetry act = act <|> nop 
