{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Posix.Env.ByteString
import System.Posix.Files.ByteString (removeLink)
import System.INotify
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Foreign.C.Types (CTime)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad (when, forever, guard)

data FileEvent = FileEvent { time :: EpochTime
                           , path :: ByteString
                           } deriving (Show)

type Trash = ByteString -> STM ()

data Watch = Watch { maxAge       :: CTime
                   , eventQueue   :: TQueue FileEvent
                   , eventInHand  :: TVar (Maybe FileEvent)
                   , purgeEnabled :: TVar Bool
                   }

data WatchOut = WatchOut { watchData    :: Watch
                         , watchDesc    :: WatchDescriptor
                         , purgeThread  :: ThreadId
                         }

main = do
  [dir] <- getArgs
  trashQueue <- newTQueueIO
  let trash = writeTQueue trashQueue
  withINotify $ \ih -> do
    w <- forkWatch trash maxAge ih dir
    pure ()
  pure ()
  where maxAge = 5

-- |Start motion
startMotion :: Watch -> STM ()
startMotion Watch{..} = writeTVar purgeEnabled False

-- |Stop motion, returns video files TODO encoded video.
stopMotion :: Watch -> STM [ByteString]
stopMotion Watch{..} = do
  purging <- readTVar purgeEnabled
  if purging
    then pure $ error "motionStop called without motionStart"
    else do writeTVar purgeEnabled True
            -- Put the item back if there's any
            readTVar eventInHand >>= maybe nop (unGetTQueue eventQueue)
            -- Get the contents
            map path <$> flushTQueue eventQueue

-- |Stop watching. Undefined behaviour if called twice.
stopWatch :: Trash -> WatchOut -> IO ()
stopWatch trash WatchOut{..} = do
  let Watch{..} = watchData
  removeWatch watchDesc
  killThread purgeThread
  atomically $ do
    readTVar eventInHand >>= maybe nop (trash . path)
    flushTQueue eventQueue >>= mapM_ (trash . path)

forkWatch :: Trash -> CTime -> INotify -> ByteString -> IO WatchOut
forkWatch trash maxAge ih dir = do
  eventQueue <- newTQueueIO
  eventInHand <- newTVarIO Nothing
  purgeEnabled <- newTVarIO True -- Motion stopped at start
  let watchData = Watch{..}
  
  purgeThread <- forkIO $ forever $ purgeMsg trash watchData
  let eh = atomically . writeTQueue eventQueue
  watchDesc <- watchClosures ih eh dir
  pure WatchOut{..}

-- |Watch given directory, enqueuing events as they occur.
watchClosures :: INotify -> (FileEvent -> IO ()) -> ByteString -> IO WatchDescriptor
watchClosures h sendEvent dir = addWatch h [CloseWrite] dir handleEvent
  where handleEvent = maybe nop send . toClosedFile
        send file = eventify dir file >>= sendEvent

-- |Convert directory name, file name and current time to FileEvent
eventify :: ByteString -> ByteString -> IO FileEvent
eventify dir file = do
  time <- epochTime
  let path = B.concat [dir, "/", file]
  pure FileEvent{..}

-- |Converts Event to file name of the closed file or Nothing if it's
-- something else.
toClosedFile :: Event -> Maybe ByteString
toClosedFile Closed{..} = if isDirectory then Nothing else maybeFilePath
toClosedFile _ = Nothing

-- |Purge old file if we have permission to do so. Has a problem in
-- where an event might be lost if startMotion and stopMotion occur
-- between two transactions.
purgeMsg :: Trash -> Watch -> IO ()
purgeMsg trash Watch{..} = do
  FileEvent{..} <- atomically $ do
    -- Keep going only if purging is enabled
    readTVar purgeEnabled >>= guard
    -- Take one item and place it to "hand" to avoid race conditions.
    a <- readTQueue eventQueue
    writeTVar eventInHand $ Just a
    pure a

  -- Create delay action to span over the expiration time
  delay <- do
    now <- epochTime
    let expiresIn = time - now + maxAge
    delayGuard $ 1000000 * fromEnum expiresIn

  -- File removal at expiration time or falltrough
  atomically $ do
    enabled <- readTVar purgeEnabled
    when enabled $ do
      -- Ensure delay spent and add item to remove queue and release it.
      delay
      trash path
      writeTVar eventInHand Nothing

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
