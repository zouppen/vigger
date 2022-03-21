{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Watch ( Watch(..)
             , startCapture
             , stopCapture
             , forkWatch
             , stopWatch
             ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.INotify
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Foreign.C.Types (CTime)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (when, forever, guard)
import Data.Foldable (traverse_)

import Exceptions
import Trasher (Trash)

data FileEvent = FileEvent { time :: EpochTime
                           , path :: ByteString
                           } deriving (Show)

data Watch = Watch { eventQueue   :: TQueue FileEvent
                   , eventInHand  :: TVar (Maybe FileEvent)
                   , purgeEnabled :: TVar Bool
                   , watchDesc    :: WatchDescriptor
                   , purgeThread  :: ThreadId
                   , lastEnqueue  :: IO EpochTime
                   }

-- |Start capture
startCapture :: Watch -> STM ()
startCapture Watch{..} = writeTVar purgeEnabled False

-- |Stop capture, returns file names
stopCapture :: Watch -> STM [ByteString]
stopCapture watch@Watch{..} = do
  purging <- readTVar purgeEnabled
  when purging $ throwSTM $ ViggerNonFatal "motionStop called without motionStart"
  -- Everything is fine, let's starting the purge again
  writeTVar purgeEnabled True
  -- Get the contents
  takeFiles watch

-- |Stop watching. Undefined behaviour if called twice.
stopWatch :: Trash -> Watch -> IO ()
stopWatch trash watch@Watch{..} = do
  removeWatch watchDesc
  killThread purgeThread
  atomically $ takeFiles watch >>= traverse_ trash

-- |Takes files from the queue. Not for external use!
takeFiles :: Watch -> STM [ByteString]
takeFiles Watch{..} = do
  events <- glue <$> flushTQueue eventQueue <*> readTVar eventInHand
  pure $ map path events
  where glue a = maybe a (:a)

-- |Start watching given directory with wiven purge timeout.
forkWatch :: Trash -> CTime -> INotify -> ByteString -> IO Watch
forkWatch trash maxAge ih dir = do
  eventQueue <- newTQueueIO
  eventInHand <- newTVarIO Nothing
  purgeEnabled <- newTVarIO True -- Motion stopped at start
  -- Initialize dead man switch with current time
  lastEnqueueVar <- epochTime >>= newTVarIO
  let lastEnqueue = readTVarIO lastEnqueueVar
  purgeThread <- forkIO $ forever $ purgeEvent
    maxAge
    trash
    (readTQueue eventQueue)
    (writeTVar eventInHand)
    (readTVar purgeEnabled)
  watchDesc <- watchClosures ih dir $ \path -> do
    time <- epochTime
    atomically $ do
      writeTQueue eventQueue FileEvent{..}
      writeTVar lastEnqueueVar time
  pure Watch{..}

-- |Watch given directory, enqueuing closed files as they occur.
watchClosures :: INotify -> ByteString -> (ByteString -> IO ()) -> IO WatchDescriptor
watchClosures h dir send = addWatch h [CloseWrite] dir handleEvent
  where handleEvent = maybe nop action . toClosedFile
        action file = send $ B.concat [dir, "/", file]

-- |Converts Event to file name of the closed file or Nothing if it's
-- something else.
toClosedFile :: Event -> Maybe ByteString
toClosedFile Closed{..} = if isDirectory then Nothing else maybeFilePath
toClosedFile _ = Nothing

-- |Purge old file if we have permission to do so. Has a problem in
-- where an event might be lost if startMotion and stopMotion occur
-- between two transactions.
purgeEvent :: CTime
           -> (ByteString -> STM a)
           -> STM FileEvent
           -> (Maybe FileEvent -> STM ())
           -> STM Bool
           -> IO ()
purgeEvent maxAge trash get hold purgeEnabled = do
  FileEvent{..} <- atomically $ do
    -- Keep going only if purging is enabled
    purgeEnabled >>= guard
    -- Take one item and place it to "hand" to avoid race conditions.
    a <- get
    hold $ Just a
    pure a

  -- Create delay action to span over the expiration time
  delay <- do
    now <- epochTime
    let expiresIn = time - now + maxAge
    delayGuard $ 1000000 * fromEnum expiresIn

  -- File removal at expiration time or falltrough
  atomically $ do
    enabled <- purgeEnabled
    when enabled $ do
      -- Ensure delay spent and add item to remove queue and release it.
      delay
      trash path
      hold Nothing

-- |Retuns STM action which retries until given timeout reached
delayGuard :: Int -> IO (STM ())
delayGuard delay
  | delay > 0 = do
      var <- registerDelay delay
      pure $ readTVar var >>= guard
  | otherwise = pure nop

-- |Shorthand for doing nothing
nop :: Applicative m => m ()
nop = pure ()
