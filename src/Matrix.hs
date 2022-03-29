module Matrix where

import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict (Map, elems)
import Data.Text.Lazy (Text)
import System.FilePath.ByteString
import System.Process (waitForProcess)

import Ffmpeg

-- |Send message to Matrix. May block for some time so consider
-- running with forkIO to make it work in the background.
sendToMatrix :: Text -> STM (Map Text RawFilePath) -> IO ()
sendToMatrix triggerName fileAct = do
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
  -- TODO process the output
  putStrLn $ show $ fst <$> videos
  pure ()
