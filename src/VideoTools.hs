{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module VideoTools where

import Control.Concurrent.STM
import Control.Monad (guard)
import Data.Map.Strict (Map, elems)
import Data.Text.Lazy (Text)
import System.FilePath.ByteString (RawFilePath, decodeFilePath)
import Data.Time.LocalTime (ZonedTime)
import System.Process (waitForProcess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map.Strict (Map, toList, fromList)
import Control.Concurrent.Async (forConcurrently)

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
-- temporary files and the caller must clean them.
snapshotFrame :: STM (Map Text RawFilePath) -> IO (Map Text FilePath)
snapshotFrame fileAct = do
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

-- |Render given TrigerData to a video and store it under
-- recordingPath.
renderVideos :: Config -> TriggerData -> IO (Map Text FilePath)
renderVideos Config{..} TriggerData{..} =
  fromList <$> forConcurrently (toList videoFiles) renderVideo
  where
    startFormat fmt = pure $ pack $ formatTime defaultTimeLocale (unpack fmt) startTime
    renderVideo (cameraName, Capture{..}) = do
      let subst = toSubstituter [ f0 "camera" cameraName
                                , f0 "trigger" triggerName
                                , f1 "start" startFormat
                                ]
      -- Prepare directory hierarchy for the video file
      let outfile = unpack $ substitute subst recordingPath
      createDirectoryIfMissing True $ takeDirectory outfile
      -- Encode video and remove files afterwards
      composeVideo outfile captureFiles
      atomically captureClean
      pure (cameraName, outfile)
