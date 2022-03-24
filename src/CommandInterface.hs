{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module CommandInterface (commandInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Exception (IOException, try, throw)
import Data.Function (fix)
import Data.Map.Strict (Map, (!?), keys, foldlWithKey)
import System.IO.Temp (emptySystemTempFile)

import Exceptions
import Ffmpeg
import Loader
import Watch

-- |The loop which is running when the system is operating nominally.
commandInterface :: Map String TriggerOp -> IO ()
commandInterface w = do
  putStrLn "Vigger command line interface. Type \"help\" for instructions"
  fix $ \loop -> do
    ret <- try $ cmdHandler w
    case ret of
      Left ViggerStop -> putStrLn "Quitting..."
      Left (ViggerNonFatal msg) -> do
        putStrLn $ "Error while running command: " <> msg
        loop
      Right _ -> loop

-- |Process a single command. Throws ViggerExceptions.
cmdHandler :: Map String TriggerOp -> IO ()
cmdHandler w = do
  cmd <- getCmd
  case cmd of
    ["list"] -> putStr $ unlines $ map ("- " <>) $ keys w
    ["start", key] -> case w !? key of
      Just TriggerOp{..} -> do
        motionStart
        putStrLn $ "Motion started on " <> key
      Nothing -> putStrLn errMsg
    ["stop", key] -> case w !? key of
      Just TriggerOp{..} -> do
        -- Encode video and remove files afterwards
        motion <- motionEnd
        videos <- forConcurrently motion $ \Capture{..} -> do
          outfile <- emptySystemTempFile "vigger.mp4"
          composeVideo outfile captureFiles
          atomically captureClean
          pure outfile
        putStrLn $ "Motion stopped on " <> key <> ". Got videos: " <> show videos
      Nothing -> putStrLn errMsg
    ["status"] -> traverse cameraState w >>= putStr . formatCameraStates
    ["help"] -> putStr $ unlines
      [ "  list: Lists all triggers"
      , "  start TRIGGER: Start motion"
      , "  stop TRIGGER: Stop motion"
      , "  status: Print timestamps of last camera video fragments"
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
  line <- try getLine
  either (ioeConst $ throw ViggerStop) (pure . words) line
