{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Loader ( Stuff(..)
              , TriggerOp(..)
              , TriggerData(..)
              , withStuff
              , initTriggers
              ) where

import System.INotify
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Foldable (traverse_)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Control.Concurrent (killThread, forkIO)
import Control.Applicative

import Config
import Watch
import Trasher
import Ffmpeg
import Deadman
import Exceptions

-- |Initialized stuff, intended to be used as a singleton, initialized
-- with `withStuff`.
data Stuff = Stuff { ih     :: INotify
                   , trash  :: Trash
                   }

-- |Camera consists of video splitter and file watcher
data CameraOp = CameraOp { watch        :: Watch
                         , splitterStop :: IO ()
                         }

-- |Operations for a single trigger.
data TriggerOp = TriggerOp { motionStart :: IO ()
                           , motionEnd   :: IO TriggerData
                           , shutdown    :: IO ()
                           , cameraState :: IO (Map String FileEvent)
                           }

data TriggerData = TriggerData { startTime  :: ZonedTime
                               , videoFiles :: Map String Capture
                               }

-- |Initialize all the bells and whistles and run the action.
withStuff :: (Stuff -> IO ()) -> IO ()
withStuff act = do
  withINotify $ \ih -> do
    withTrasher $ \trash -> do
      act Stuff{..}

-- |Initialize everything for the command interface, with a map
-- of possible operations.
initTriggers :: Config -> Stuff -> IO (Map String TriggerOp)
initTriggers Config{..} stuff = traverse (forkTrigger stuff) triggers

-- |Fork a trigger, which is be composed of one or more cameras.
forkTrigger :: Stuff -> Trigger -> IO TriggerOp
forkTrigger stuff Trigger{..} = do
  ops <- traverse (forkCamera stuff) cameras
  startVar <- newEmptyTMVarIO
  -- Collect actions
  let motionStart = do
        now <- getZonedTime
        atomically $ do
          putTMVar startVar now <|> throwSTM (ViggerNonFatal "Already triggered")
          traverse_ (startCapture.watch) ops
      motionEnd = atomically $ do
        startTime <- takeTMVar startVar <|> throwSTM (ViggerNonFatal "Trigger already stopped")
        videoFiles <- traverse (stopCapture.watch) ops
        pure TriggerData{..}
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
