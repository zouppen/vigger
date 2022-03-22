{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Loader ( Stuff(..)
              , TriggerOp(..)
              , withStuff
              , initTriggers
              ) where

import System.INotify
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Foldable (traverse_)
import Control.Concurrent (killThread, forkIO)

import Config
import Watch
import Trasher
import Ffmpeg
import Deadman

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
                           , motionEnd   :: IO (Map String Capture)
                           , shutdown    :: IO ()
                           , cameraState :: IO (Map String FileEvent)
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
