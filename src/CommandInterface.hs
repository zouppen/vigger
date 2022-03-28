{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module CommandInterface (commandInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Data.Function (fix)
import Data.Map.Strict (Map, (!?), keys, foldMapWithKey)
import Data.Text (Text, pack, unpack)
import System.IO.Temp (emptySystemTempFile)

import Config (Config(..))
import Exceptions
import Loader
import Watch

-- |The loop which is running when the system is operating nominally.
commandInterface :: Config -> Map Text TriggerOp -> IO ()
commandInterface config ops = bracket start stop $ const $ viggerLoopCatch $ do
  cmd <- getCmd
  case cmd of
    ["list"] -> putStr $ unlines $ map (("- " <> ).unpack) $ keys ops
    ["start", key] -> case ops !? (pack key) of
      Just TriggerOp{..} -> do
        motionStart
        putStrLn $ "Motion started on " <> key
      Nothing -> putStrLn errMsg
    ["stop", key] -> case ops !? (pack key) of
      Just TriggerOp{..} -> do
        -- Encode video and remove files afterwards
        td@TriggerData{..} <- motionEnd
        videos <- renderVideos config td
        putStrLn $ "Motion stopped on " <> key <> ". Started at " <> show startTime <> ". Got videos: " <> show videos
      Nothing -> putStrLn errMsg
    ["status"] -> traverse cameraState ops >>= putStr . formatCameraStates
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
        -- We currently do no resource initialization so start and
        -- stop are super simple
        start = putStrLn "Vigger command line interface. Type \"help\" for instructions"
        stop = const $ pure ()

formatCameraStates :: Map Text (Map Text FileEvent) -> String
formatCameraStates = foldMapWithKey formatTrigger
  where formatTrigger k m = "  " <> unpack k <> ":\n" <> foldMapWithKey formatState m
        formatState k v = "    " <> unpack k <> ":\n" <> formatEvent v
        formatEvent FileEvent{..} =  "      time: "  <> show time <> "\n      path: " <> show path <> "\n"

-- |Gets a command and splits it into words. If EOF reached, returns ["quit"].
getCmd :: IO [String]
getCmd = do
  line <- try getLine
  either (ioeConst $ throw ViggerStop) (pure . words) line
