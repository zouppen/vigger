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
                   , watchDesc    :: WatchDescriptor
                   , purgeThread  :: ThreadId
                   }

main = do
  [dir] <- getArgs
  q <- newTQueueIO
  withINotify $ \ih -> do
    let eh = atomically . writeTQueue q
    w <- watchClosures ih eh dir
    captureState <- pure $ pure Nothing --readTVar <$> newTVarIO Nothing
    let dequeue = readTQueue q
    forever $ processMsg maxAge dequeue captureState
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
stopWatch :: Trash -> Watch -> IO ()
stopWatch trash Watch{..} = do
  removeWatch watchDesc
  killThread purgeThread
  atomically $ do
    readTVar eventInHand >>= maybe nop (trash . path)
    flushTQueue eventQueue >>= mapM_ (trash . path)

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

-- |Process a single message. Given file is removed after maxAge if "put" is Nothing.
processMsg :: CTime -> STM FileEvent -> STM (Maybe (ByteString -> STM ())) -> IO ()
processMsg maxAge get put = do
  -- Get the next item
  FileEvent{..} <- atomically get

  -- Adding delay if it hasn't yet expired
  delayState <- do
    now <- epochTime
    let expiresIn = time - now + maxAge
    if expiresIn > 0
      then readTVar <$> registerDelay (1000000 * fromEnum expiresIn)
      else pure $ pure True -- No delay

  -- Perform remove or "keep" operation based on the state
  join $ atomically $ do
    capture <- put
    expired <- delayState
    case (capture, expired) of
      -- If we collect, move it to that queue
      (Just enq, _) -> do
        enq path
        pure nop
      -- If we don't collect, drop the file after the delay
      (_, True) -> pure $ removeLink path
      -- Otherwise we wait
      _ -> retry

-- |Shorthand for doing nothing
nop :: Applicative m => m ()
nop = pure ()
