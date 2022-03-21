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
import Control.Concurrent (killThread, forkIO)

import Config
import Watch
import Trasher
import Ffmpeg
import Exceptions
import Deadman

data Stuff = Stuff { ih     :: INotify
                   , trash  :: Trash
                   , tmpDir :: FilePath
                   }

-- |Camera consists of video splitter and file watcher
data CameraOp = CameraOp { watch        :: Watch
                         , splitterStop :: IO ()
                         }

-- |Operations for a single trigger.
data TriggerOp = TriggerOp { motionStart :: IO ()
                           , motionEnd   :: IO (Map String [B.ByteString])
                           , shutdown    :: IO ()
                           }

main = do
  Config{..} <- parseCommandAndConfig
  withStuff $ \stuff -> do
    -- Start everything
    triggerOps <- traverse (forkTrigger stuff) triggers
    -- Handle incoming events
    putStrLn "Up and running..."
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
        splitterStop
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
  let start = startVideoSplit dir url
  splitterStop <- case timeout of
    Just timeout -> do
      tid <- forkIO $ deadManLoop
        (toEnum timeout)
        (lastEnqueue watch)
        start
        stopVideoSplit
      pure $ killThread tid
    Nothing -> do
      procH <- start
      pure $ stopVideoSplit procH
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
    ["list"] -> putStr $ unlines $ map ("- " ++)  $ keys w
    ["start", key] -> case w !? key of
      Just TriggerOp{..} -> do
        motionStart
        putStrLn $ "Motion started on " ++ key
      Nothing -> putStrLn errMsg
    ["stop", key] -> case w !? key of
      Just TriggerOp{..} -> do
        files <- motionEnd
        putStrLn $ "Motion stopped on " ++ key ++ ". Got files: " ++ show files
      Nothing -> putStrLn errMsg
    ["help"] -> putStr $ unlines
      [ "  list: Lists all triggers"
      , "  start TRIGGER: Start motion"
      , "  stop TRIGGER: Stop motion"
      , "  quit: Quit. Ctrl+D also works."
      ]
    ["quit"] -> throw ViggerStop
    _ -> putStrLn "Unknown command. Type \"help\" to see list of commands"
  where errMsg = "Trigger not found. Type \"list\" to see them all"
                     
-- |Gets a command and splits it into words. If EOF reached, returns ["quit"].
getCmd :: IO [String]
getCmd = do
  line <- try getLine :: IO (Either IOException String)
  either (throw ViggerStop) (pure . words) line
