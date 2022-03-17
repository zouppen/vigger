{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import System.INotify
import Control.Concurrent.STM
import Data.ByteString.UTF8 (fromString)
import System.IO.Temp
import qualified Data.Map.Strict as M
import Data.Foldable (traverse_)

import Config
import Watch
import Trasher
import Ffmpeg

data Stuff = Stuff { ih     :: INotify
                   , trash  :: Trash
                   , tmpDir :: FilePath
                   }

-- |Camera consists of video splitter and file watcher
data CameraOp = CameraOp { watch    :: Watch
                         , splitter :: ProcessHandle
                         }

-- |Operations for a single trigger.
data TriggerOp = TriggerOp { motionStart :: IO ()
                           , motionEnd   :: IO (M.Map String [B.ByteString])
                           , shutdown    :: IO ()
                           }

main = do
  conf@Config{..} <- parseCommandAndConfig
  print conf
  withStuff $ \stuff -> do
    -- Start everything
    triggerOps <- traverse (forkTrigger stuff) triggers
    -- TODO event loop
    getLine
    pure ()
    -- Shutting down operations
    traverse_ shutdown triggerOps

-- |Initialize all the bells and whistles and run the action.
withStuff :: (Stuff -> IO ()) -> IO ()
withStuff act = do
  withINotify $ \ih -> do
    withSystemTempDirectory "vigger" $ \tmpDir -> do
      withTrasher $ \trash -> do
        act Stuff{..}

-- |Fork a trigger, which is be composed of one or more cameras.
forkTrigger :: Stuff -> Trigger -> IO TriggerOp
forkTrigger stuff Trigger{..} = do
  ops <- traverse (forkCamera stuff) cameras
  -- Collect actions
  let motionStart = atomically $ traverse_ (startCapture.watch) ops
      motionEnd = atomically $ traverse (stopCapture.watch) ops
      shutdown = flip traverse_ ops $ \CameraOp{..} -> do
        -- Stop FFmpeg
        stopVideoSplit splitter
        -- Stop watches
        stopWatch (trash stuff) watch
  pure TriggerOp{..}

-- |Fork video splitter and cleaner for an individual camera.
forkCamera :: Stuff -> Camera -> IO CameraOp
forkCamera Stuff{..} Camera{..} = do
  -- Create temporary directory for files inside the master temp dir.
  dir <- createTempDirectory tmpDir "camera"
  -- Start change watcher
  watch <- forkWatch trash (toEnum precapture) ih $ fromString dir
  -- Start video splitter with FFmpeg
  splitter <- startVideoSplit dir url
  -- Return handle
  pure CameraOp{..}

{-
  [dir] <- getArgs
  (_, trash) <- runTrasher
  withINotify $ \ih -> do
    w <- forkWatch trash maxAge ih dir
    cmdLoop w
    stopWatch trash w
  pure ()
  where maxAge = 5
-}

cmdLoop :: Watch -> IO ()
cmdLoop w = do
  cmd <- getLine
  case cmd of
    "start" -> do
      atomically $ startCapture w
      putStrLn "Motion started"
      cmdLoop w
    "stop" -> do
      files <- atomically $ stopCapture w
      putStrLn $ "Motion stopped " ++ show files
      cmdLoop w
    "quit" -> do
      putStrLn "Quitting"
    _ -> do
      putStrLn "Unknown command"
      cmdLoop w
