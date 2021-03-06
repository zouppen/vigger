{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module CommandInterface (commandInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Data.Map.Strict (Map, (!?), keys, foldMapWithKey, traverseWithKey)
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy.IO as T

import Config (Config(..), Matrix(..))
import Exceptions
import Loader
import Watch
import VideoTools (renderVideos, storeJpegMap)
import Matrix
import Ffmpeg (jpegPayload)

-- |The loop which is running when the system is operating nominally.
commandInterface :: Config -> MatrixConn -> Map Text TriggerOp -> IO ()
commandInterface config matrixConn ops = bracket start stop $ const $ viggerLoopCatch $ do
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
    ["snapshot", key] -> case ops !? (pack key) of
      Just TriggerOp{..} -> do
        putStrLn $ "Triggering snapshot on " <> key
        payload <- trigSnapshot
        files <- storeJpegMap config (pack key) payload
        putStr $ flip foldMapWithKey files $ \cam file -> "  " <> show cam <> ": " <> file <> "\n"
      Nothing -> putStrLn errMsg
    ["matrix", key] -> case ops !? (pack key) of
      Just TriggerOp{..} -> do
        let matrixRoom = room $ matrix config
        putStrLn $ "Sending Matrix message on " <> key
        payload <- trigSnapshot
        out <- flip traverseWithKey payload $ \cam jpeg ->
          sendImage matrixConn matrixRoom cam (jpegPayload jpeg)
        print out
      Nothing -> putStrLn errMsg
    ["status"] -> traverse cameraState ops >>= T.putStr . formatCameraStates
    ["help"] -> putStr $ unlines
      [ "  list: Lists all triggers"
      , "  start TRIGGER: Start motion"
      , "  stop TRIGGER: Stop motion"
      , "  snapshot TRIGGER: Take a snapshot from given trigger"
      , "  matrix TRIGGER: Take a snapshot from given trigger and send to Matrix"
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

formatCameraStates :: Map Text (Map Text FileEvent) -> Text
formatCameraStates = foldMapWithKey formatTrigger
  where formatTrigger k m = "  " <> k <> ":\n" <> foldMapWithKey formatState m
        formatState k v = "    " <> k <> ":\n" <> formatEvent v
        formatEvent FileEvent{..} =  "      time: "  <> pack (show time) <> "\n      path: " <> pack (show path) <> "\n"

-- |Gets a command and splits it into words. If EOF reached, returns ["quit"].
getCmd :: IO [String]
getCmd = do
  line <- try getLine
  either (ioeConst $ throw ViggerStop) (pure . words) line
