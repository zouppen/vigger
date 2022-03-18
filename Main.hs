{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import System.INotify
import Control.Concurrent.STM
import Data.ByteString.UTF8 (fromString)
import System.IO.Temp (withSystemTempDirectory, createTempDirectory)
import Data.Map.Strict (Map, (!?), keys)
import Data.Foldable (traverse_)
import Control.Exception (IOException, try, throw)

import Config
import Watch
import Trasher
import Ffmpeg
import Exceptions

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
                           , motionEnd   :: IO (Map String [B.ByteString])
                           , shutdown    :: IO ()
                           }

main = do
  conf@Config{..} <- parseCommandAndConfig
  print conf
  withStuff $ \stuff -> do
    -- Start everything
    triggerOps <- traverse (forkTrigger stuff) triggers
    -- Handle incoming events
    cmdLoop triggerOps
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

-- |The loop which is running when the system is operating nominally.
cmdLoop :: Map String TriggerOp -> IO ()
cmdLoop w = do
  ret <- try $ cmdHandler w
  case ret of
    Left ViggerStop -> putStrLn "Quitting..."
    Left (ViggerNonFatal msg) -> do
      putStrLn $ "Error while running command: " ++ msg
      loop
    Right _ -> loop
  where loop = cmdLoop w

-- |Process a single command. Throws ViggerExceptions.
cmdHandler :: Map String TriggerOp -> IO ()
cmdHandler w = do
  cmd <- getCmd
  case cmd of
    ["list"] -> putStr $ unlines $ keys w
    ["start", key] -> case w !? key of
      Just TriggerOp{..} -> do
        motionStart
        putStrLn $ "Motion started on " ++ key
      Nothing -> putStrLn "Trigger not found"
    ["stop", key] -> case w !? key of
      Just TriggerOp{..} -> do
        files <- motionEnd
        putStrLn $ "Motion stopped on " ++ key ++ ". Got files: " ++ show files
      Nothing -> putStrLn "Trigger not found"
    ["quit"] -> throw ViggerStop
    _ -> putStrLn "Unknown command"
                     
-- |Gets a command and splits it into words. If EOF reached, returns ["quit"].
getCmd :: IO [String]
getCmd = do
  line <- try getLine :: IO (Either IOException String)
  either (throw ViggerStop) (pure . words) line

