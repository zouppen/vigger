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
import Control.Monad (forever, join)

data WatchDir = WatchDir { queue   :: TQueue FileEvent
                         , watcher :: WatchDescriptor
                         }

data FileEvent = FileEvent { time :: EpochTime
                           , path :: ByteString
                           } deriving (Show)

main = do
  [dir] <- getArgs
  withINotify $ \ih -> do
    w <- watch ih dir
    captureState <- pure $ pure Nothing --readTVar <$> newTVarIO Nothing
    processQueue maxAge (queue w) captureState
  pure ()
  where maxAge = 5

-- |Watch given directory, enqueuing events as they occur.
watch :: INotify -> ByteString -> IO WatchDir
watch h dir = do
  queue <- newTQueueIO
  watcher <- addWatch h [CloseWrite] dir $ handler $ enqueue queue dir
  pure WatchDir{..}

-- |Enqueue event, used by handler to make it implementation neutral.
enqueue :: TQueue FileEvent -> ByteString -> ByteString -> IO ()
enqueue q dir file = do
  time <- epochTime
  let path = B.concat [dir, "/", file]
  atomically $ writeTQueue q FileEvent{..}

-- |Perform given task with file name if Event has correct type.
handler :: Applicative f => (ByteString -> f ()) -> Event -> f ()
handler enq e = case e of
  Closed{..} -> case (isDirectory, maybeFilePath) of
    (False, Just file) -> enq file
    _ -> pure ()
  _ -> pure ()

-- |Purge old ones
processQueue :: CTime -> TQueue FileEvent -> STM (Maybe (ByteString -> STM ())) -> IO ()
processQueue maxAge q captureState = forever $ do
  -- Get the next item
  FileEvent{..} <- atomically $ readTQueue q

  -- Adding delay if it hasn't yet expired
  delayState <- do
    now <- epochTime
    let expiresIn = time - now + maxAge
    if expiresIn > 0
      then readTVar <$> registerDelay (1000000 * fromEnum expiresIn)
      else pure $ pure True -- No delay

  -- Perform remove or "keep" operation based on the state
  join $ atomically $ do
    capture <- captureState
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
