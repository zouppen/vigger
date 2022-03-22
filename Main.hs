{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import System.INotify
import Control.Concurrent.STM
import Data.Map.Strict (Map, (!?), keys, foldlWithKey)
import Data.Foldable (traverse_)
import Control.Exception (IOException, try, throw)
import Control.Concurrent (killThread, forkIO)
import System.IO.Temp (emptySystemTempFile)

import Config
import Watch
import Trasher
import Ffmpeg
import Exceptions
import Deadman

data Stuff = Stuff { ih     :: INotify
                   , trash  :: Trash
                   }

-- |Camera consists of video splitter and file watcher
data CameraOp = CameraOp { watch        :: Watch
                         , splitterStop :: IO ()
                         }

-- |Operations for a single trigger.
data TriggerOp = TriggerOp { motionStart :: IO ()
                           , motionEnd   :: IO (Map String [RawFilePath])
                           , shutdown    :: IO ()
                           , cameraState :: IO (Map String FileEvent)
                           }

main = do
  Config{..} <- parseCommandAndConfig
  withStuff $ \stuff -> do
    -- Start everything
    triggerOps <- traverse (forkTrigger stuff) triggers
    -- Handle incoming events
    putStrLn "Up and running..."
    cmdLoop stuff triggerOps
    -- Shutting down operations
    traverse_ shutdown triggerOps

-- |Initialize all the bells and whistles and run the action.
withStuff :: (Stuff -> IO ()) -> IO ()
withStuff act = do
  withINotify $ \ih -> do
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
        splitterStop
        -- Stop watches
        stopWatch watch
      cameraState = traverse (lastEnqueue . watch) ops
  pure TriggerOp{..}

-- |Fork video splitter and cleaner for an individual camera.
forkCamera :: Stuff -> Camera -> IO CameraOp
forkCamera Stuff{..} Camera{..} = do
  -- Start change watcher
  watch <- forkWatch trash (toEnum precapture) ih
  -- Start video splitter with FFmpeg
  let start = startVideoSplit (workDir watch) url
  splitterStop <- case timeout of
    Just timeout -> do
      tid <- forkIO $ deadManLoop
        (toEnum timeout)
        (time <$> lastEnqueue watch)
        start
        stopVideoSplit
      pure $ killThread tid
    Nothing -> do
      procH <- start
      pure $ stopVideoSplit procH
  -- Return handle
  pure CameraOp{..}

-- |The loop which is running when the system is operating nominally.
cmdLoop :: Stuff -> Map String TriggerOp -> IO ()
cmdLoop stuff w = do
  ret <- try $ cmdHandler stuff w
  case ret of
    Left ViggerStop -> putStrLn "Quitting..."
    Left (ViggerNonFatal msg) -> do
      putStrLn $ "Error while running command: " ++ msg
      loop
    Right _ -> loop
  where loop = cmdLoop stuff w

-- |Process a single command. Throws ViggerExceptions.
cmdHandler :: Stuff -> Map String TriggerOp -> IO ()
cmdHandler Stuff{..} w = do
  cmd <- getCmd
  case cmd of
    ["list"] -> putStr $ unlines $ map ("- " ++)  $ keys w
    ["start", key] -> case w !? key of
      Just TriggerOp{..} -> do
        motionStart
        putStrLn $ "Motion started on " ++ key
      Nothing -> putStrLn errMsg
    ["stop", key] -> case w !? key of
      Just TriggerOp{..} -> do
        -- Encode video and remove files afterwards
        motion <- motionEnd
        videos <- flip traverse motion $ \files -> do
          outfile <- emptySystemTempFile "vigger.mp4"
          composeVideo outfile files
          trashIO trash files
          pure outfile
        putStrLn $ "Motion stopped on " ++ key ++ ". Got videos: " ++ show videos
      Nothing -> putStrLn errMsg
    ["cameras"] -> traverse cameraState w >>= putStr . formatCameraStates
    ["help"] -> putStr $ unlines
      [ "  list: Lists all triggers"
      , "  start TRIGGER: Start motion"
      , "  stop TRIGGER: Stop motion"
      , "  cameras: Print timestamps of last camera video fragments"
      , "  quit: Quit. Ctrl+D also works."
      ]
    ["quit"] -> throw ViggerStop
    _ -> putStrLn "Unknown command. Type \"help\" to see list of commands"
  where errMsg = "Trigger not found. Type \"list\" to see them all"

formatCameraStates :: Map String (Map String FileEvent) -> String
formatCameraStates = foldlWithKey formatTrigger ""
  where formatTrigger a k m = a <> "  " <> k <> ":\n" <> foldlWithKey formatState "" m
        formatState a k v = a <> "    " <> k <> ":\n" <> formatEvent v
        formatEvent FileEvent{..} =  "      time: "  <> show time <> "\n      path: " <> show path <> "\n"

-- |Gets a command and splits it into words. If EOF reached, returns ["quit"].
getCmd :: IO [String]
getCmd = do
  line <- try getLine :: IO (Either IOException String)
  either (throw ViggerStop) (pure . words) line
