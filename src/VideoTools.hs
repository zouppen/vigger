{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- |Video utils which are more tied to our data structures than those
-- in Ffmpeg.
module VideoTools where

import Control.Concurrent.STM
import Control.Monad (guard)
import Data.Map.Strict (Map, elems, mapWithKey)
import Data.Text.Lazy (Text)
import System.FilePath.ByteString (RawFilePath, decodeFilePath)
import Data.Time.LocalTime (ZonedTime)
import System.Process (waitForProcess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Map.Strict (Map)
import Control.Concurrent.Async (mapConcurrently, forConcurrently)

import Config
import Ffmpeg
import Watch (Capture(..))
import Formatter

data TriggerData = TriggerData { startTime   :: ZonedTime
                               , triggerName :: Text
                               , videoFiles  :: Map Text Capture
                               }

-- |Collect a frame snapshot. May block for some time so consider
-- running with forkIO to make it work in the background. Returns new
-- temporary files and the caller must clean them. You don't probably
-- want to call this but trigSnapshot from Loader.
snapshotFrame :: STM (Map Text RawFilePath) -> IO (Map Text FilePath)
snapshotFrame fileAct = do
  -- TODO should fork ffmpeg right when individual files are
  -- generated, without waiting for all. That would make it perform
  -- better since it could work during the waiting time.

  -- Wait new video files for each camera
  oldFiles <- atomically $ elems <$> fileAct
  files <- atomically $ do
    files <- fileAct
    -- Wait until all files have changed
    guard $ and $ zipWith (/=) oldFiles $ elems files
    pure files
  -- Generate videos concurrently and wait for completion
  videos <- traverse (takeLastFrame . decodeFilePath) files
  traverse (waitForProcess . snd) videos
  -- Return the files
  pure $ fst <$> videos

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
