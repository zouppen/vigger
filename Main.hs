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

data FileEvent = FileEvent { time :: EpochTime
                           , path :: ByteString
                           } deriving (Show)

type EventHandler m = FileEvent -> m ()

main = do
  [dir] <- getArgs
  q <- newTQueueIO
  withINotify $ \ih -> do
    let eh = atomically . writeTQueue q
    w <- watch eh ih dir
    captureState <- pure $ pure Nothing --readTVar <$> newTVarIO Nothing
    let dequeue = readTQueue q
    processQueue maxAge dequeue captureState
    pure ()
  pure ()
  where maxAge = 5

-- |Watch given directory, enqueuing events as they occur.
watch :: EventHandler IO -> INotify -> ByteString -> IO WatchDescriptor
watch eventH inotifyH dir =
  addWatch inotifyH [CloseWrite] dir $ handleEvent $ enqueue eventH dir

-- |Enqueue event, used by handler to make it implementation neutral.
enqueue :: EventHandler IO -> ByteString -> ByteString -> IO ()
enqueue h dir file = do
  time <- epochTime
  let path = B.concat [dir, "/", file]
  h FileEvent{..}

-- |Perform given task with file name if Event has correct type.
handleEvent :: Applicative f => (ByteString -> f ()) -> Event -> f ()
handleEvent enq e = case e of
  Closed{..} -> case (isDirectory, maybeFilePath) of
    (False, Just file) -> enq file
    _ -> pure ()
  _ -> pure ()

-- |Process queue. Given file is removed after maxAge if "put" is Nothing.
processQueue :: CTime -> STM FileEvent -> STM (Maybe (ByteString -> STM ())) -> IO ()
processQueue maxAge get put = forever $ do
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
