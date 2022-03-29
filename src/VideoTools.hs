{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- |Video utils which are more tied to our data structures than those
-- in Ffmpeg.
module VideoTools ( snapshotFrame
                  , renderVideos
                  , storeJpegMap
                  , TriggerData(..)
                  ) where

import Control.Concurrent.STM
import Control.Monad (guard)
import Data.Map.Strict (Map, mapWithKey, traverseWithKey)
import Data.Text.Lazy (Text)
import System.FilePath.ByteString (RawFilePath, decodeFilePath)
import Data.Time.LocalTime (ZonedTime)
import System.Process (waitForProcess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Control.Concurrent.Async (mapConcurrently, forConcurrently)
import Data.Time.LocalTime (ZonedTime, getZonedTime)

import Config
import Ffmpeg
import Watch (Capture(..))
import Formatter

data TriggerData = TriggerData { startTime   :: ZonedTime
                               , triggerName :: Text
                               , videoFiles  :: Map Text Capture
                               }

-- |Collect a frame snapshot. May block for some time because waits
-- for new videos and then renders the snapshots. You don't probably
-- want to call this directly but trigSnapshot from Loader.
snapshotFrame :: Map Text (STM RawFilePath) -> IO (Map Text Jpeg)
snapshotFrame fileActs = do
  -- Get actions Wrap the action to reference value (the startup value)
  newValueActs <- atomically $ traverse changeGuard fileActs
  -- Then run encoding as new files appear.
  forConcurrently newValueActs $ \act -> do
    file <- atomically act
    takeLastFrame $ decodeFilePath file

-- |Store given map to files
storeJpegMap :: Config -> Text -> Map Text Jpeg -> IO (Map Text FilePath)
storeJpegMap Config{..} triggerName m = do
  now <- getZonedTime
  flip traverseWithKey m $ \cameraName pic -> do
    let subst = toSubstituter [ f0 "camera" cameraName
                              , f0 "trigger" triggerName
                              , f1 "time" (pure . formatTimeText now)
                              ]
    -- Prepare directory hierarchy for the video file
    let outfile = unpack $ substitute subst snapshotPath
    createDirectoryIfMissing True $ takeDirectory outfile
    -- Store file
    writeJpeg outfile pic
    pure outfile

-- |Render given TrigerData to a video and store it based on the
-- template defined in recordingPath.
renderVideos :: Config -> TriggerData -> IO (Map Text FilePath)
renderVideos Config{..} TriggerData{..} =
  mapConcurrently renderVideo $ mapWithKey (,) videoFiles
  where
    renderVideo (cameraName, Capture{..}) = do
      let subst = toSubstituter [ f0 "camera" cameraName
                                , f0 "trigger" triggerName
                                , f1 "start" (pure . formatTimeText startTime)
                                ]
      -- Prepare directory hierarchy for the video file
      let outfile = unpack $ substitute subst recordingPath
      createDirectoryIfMissing True $ takeDirectory outfile
      -- Encode video and remove files afterwards
      composeVideo outfile captureFiles
      atomically captureClean
      pure outfile

-- |Version of formatTime which works with Text
formatTimeText :: FormatTime t => t -> Text -> Text
formatTimeText time fmt = pack $ formatTime defaultTimeLocale (unpack fmt) time

-- |Checks the given value and returns another action which retries
-- until the value is different from the initial one and returns that
-- one. Intended to run in an upcoming transaction (otherwise it never
-- changes by itself).
changeGuard :: Eq a => STM a -> STM (STM a)
changeGuard act = do
  old <- act
  pure $ do
    new <- act
    guard $ old /= new
    pure new
